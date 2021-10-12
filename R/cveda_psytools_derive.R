#!/usr/bin/env Rscript

# Copyright (c) 2017-2021 CEA
#
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software. You can use,
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info".
#
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

library(tools)
library(Psytools)
library(haven)
library(dataMaid)
library(readxl)
library(data.table)

PSYTOOLS_PSC1_DIR <- "/cveda/databank/RAW/PSC1/psytools"
PSYTOOLS_DERIVED_DIR <- "/cveda/chroot/data/tmp/psytools"
PSYTOOLS_RESOURCE_FILE <- "/cveda/databank/documentation/psytools/cVEDA_resources_en.xlsx"
PSYTOOLS_DATA_DICTIONARY_DIR <- "/cveda/databank/documentation/psytools/data_dictionary"


escape <- function(x) {
    if ("character" %in% class(x)) {
        # Escape double quotation marks by doubling them
        x <- gsub('"', '""', x)
        # Enclose in quotation marks strings with commas or quotation marks
        x <- gsub('^(.*[",\\;].*$)', '"\\1"', x)
    }
    return(x)
}


deriveData <- function(d, name) {
    # Apply relevant derivation function to each questionnaire
    if (grepl("^cVEDA-cVEDA_SOCRATIS", name)) {
        d <- deriveSOCRATIS(d)
    } else if (grepl("^cVEDA-cVEDA_EATQr_SELF", name)) {
        d <- deriveCvedaEATQr(d)
    } else if (grepl("^cVEDA-cVEDA_SST", name)) {
        d <- deriveSST(d)
    } else if (grepl("^cVEDA-cVEDA_KIRBY", name)) {
        d <- deriveKIRBY(d)
    } else if (grepl("^cVEDA-cVEDA_BART", name)) {
        d <- deriveBART(d)
    } else if (grepl("^cVEDA-cVEDA_ERT", name)) {
        d <- deriveERT(d)
    } else if (grepl("^cVEDA-cVEDA_MID", name)) {
        d <- deriveMID(d)
    } else if (grepl("^cVEDA-cVEDA_TMT", name)) {
        d <- deriveTMT(d)
    } else if (grepl("^cVEDA-cVEDA_WCST", name)) {
        d <- deriveWCST(d)
    } else if (grepl("^cVEDA-cVEDA_CORSI", name)) {
        d <- deriveCORSI(d)
    } else if (grepl("^cVEDA-cVEDA_DS", name)) {
        d <- deriveDS(d)
    } else if (grepl("^cVEDA-cVEDA_APQ", name)) {
        d <- deriveAPQ(d)
    } else if (grepl("^cVEDA-cVEDA_FHQ", name)) {
        d <- rotateQuestionnairePreserveBlock(d)
    } else if (grepl("^cVEDA-cVEDA_BIG5", name)) {
        d <- deriveBIG5(d)
    } else if (grepl("^cVEDA-cVEDA_ASSIST", name)) {
        d <- deriveASSIST(d)
    } else if (grepl("^cVEDA-cVEDA_ACEIQ", name)) {
        d <- deriveCvedaACEIQ(d)
    } else if (grepl("^cVEDA-cVEDA_IFVCS", name)) {
        d <- deriveIFVCS(d)
    } else if (grepl("^cVEDA-cVEDA_ANTHROPOMETRY", name)) {
        d <- deriveCvedaAnthropometry(d)
    } else if (grepl("^cVEDA-cVEDA_PBI", name)) {
        d <- derivePBI(d)
    } else if (grepl("^cVEDA-cVEDA_PDS", name)) {
        d <- deriveCvedaPDS(d)
    } else if (grepl("^cVEDA-cVEDA_SDQ", name)) {
        d <- deriveSDQ(d)
    } else if (grepl("^cVEDA-cVEDA_SCQ", name)) {
        d <- deriveSCQ(d)
    } else if (grepl("^cVEDA-cVEDA_SDIM", name)) {
        d <- deriveCvedaSDIM(d)
    } else if (grepl("^cVEDA-cVEDA_AAQ", name)) {
        d <- deriveAAQ(d)
    } else if (grepl("^cVEDA-cVEDA_LEQ", name)) {
        d <- deriveLEQ(d)
    } else {  # default fits all other questionnaires
        d <- rotateQuestionnaire(d)
    }

    # For the TMT the derivation script updates
    #   the completed flag to TimeOut if the task timed out
    #   it is not unknown (~1% administrations in c-VEDA) that
    #   the task was begun but was not engaged with
    #   and then restarted after the timeout.
    #   however this does enable some degree of potential practice...
    # Discard incomplete if the Ppt has ever fully completed prior to iteration selection
    if (length(d$Completed[d$Completed != "t"])) {
        d <- d[d$Completed == "t" | (d$Completed != "t" &
            !(d$User.code %in% d$User.code[d$Completed == "t"])), ]
    }
    return(d)
}


selectIterationAndSave <- function(d, iterationFunction, filepath) {
    options(datatable.print.nrows = 0)
    setDT(d)

    if(nrow(d) == 0) { return(NULL) }

    # Extract "Age.band" from "User.code"
    d$Age.band <- substring(d$User.code, nchar(d$User.code) - 1)
    setcolorder(d,ncol(d))
    # Trim "User.code" to just the PSC + FU.timepoint - we need to keep all FU timepoints attempted
    d$User.code <- substring(d$User.code, 1, nchar(d$User.code) - 3)

    # Remake iteration field if iterations exist under multiple age bands for the
    # same PSC This procedure works for long or wide data format now
    iterations <- aggregate(Iteration ~ User.code + Completed.Timestamp, FUN = head,
        1, data = d[, c("User.code", "Iteration", "Completed.Timestamp")])
    iterations <- iterations[order(iterations$User.code, iterations$Completed.Timestamp),]
    iterations$newIteration <- unlist(tapply(iterations$User.code, iterations$User.code,
        seq_along))
    iterations$Completed.Timestamp <- NULL
    setDT(iterations)
    d <- d[iterations, on = c("User.code", "Iteration")]
    d$Iteration <- d$newIteration
    d$newIteration <- NULL
    rm(iterations)

    d <- d[aggregate(Iteration ~ User.code, iterationFunction, data = d),
        on = c("User.code", "Iteration")]

    setDF(d)

    # Extract FU timepoint from "User.code"
    d$FU.timepoint<-substring(d$User.code, 13)
    d$FU.timepoint[d$FU.timepoint==""]<-"Baseline"
    setcolorder(d,ncol(d))
    
    # Trim "User.code" to just the PSC
    d$User.code <- substring(d$User.code, 1, 12)
    
    # Roll our own quoting method
    for (column in colnames(d)) {
        d[, column] <- escape(d[, column])
    }

    # Write data frame back to the processed CSV file
    columns <- sub("\\.ms\\.", "[ms]", colnames(d))  # Response time [ms]
    columns <- gsub("\\.", " ", columns)
    write.table(d, filepath, quote = FALSE, sep = ",", na = "",
        row.names = FALSE, col.names = columns)

    # Label the df from the resources file if available and also save a labelled Stata version.
    if (file.exists(PSYTOOLS_RESOURCE_FILE)) {
        require(readxl, quietly = TRUE)
        require(haven, quietly = TRUE)
        taskName <- paste(unlist(strsplit(filepath, "-"))[2], "EN", sep = "_")
        resourceSheets <- excel_sheets(PSYTOOLS_RESOURCE_FILE)
        # The resources for baseline are used for FU tasks if there are no changes
        if (!taskName %in% resourceSheets) {
            if (gsub("_FU[12]", "", taskName) %in% resourceSheets) {
                taskName <- gsub("_FU[12]", "", taskName)
            } else {
                warning(paste0("Cannot Find Resource Sheet: ", taskName))
                return()
            }
        }
        resources <- as.data.frame(read_xlsx(PSYTOOLS_RESOURCE_FILE,
            sheet = taskName, col_names = FALSE, .name_repair = make.names))
        d <- labelData(d, resources)
        write_sav(d, gsub("csv", "zsav", filepath), compress = TRUE)

        # remove the vctr class haven now uses internally - they break the makeCodeBook function
        # Must be run before MakeCodeBook if using Haven > 2.3
        for (v in names(d)) {
            class(d[,v]) = setdiff(class(d[,v]), "vctrs_vctr")
        }

        # Convert all character variables to ASCII so LaTeX doesn't blow a fuse
        for (col in names(which(sapply(d, is.character), useNames = TRUE))) {
          data.table::set(d, j = col,
                                 value = iconv(d[[col]],
                                               from = "UTF-8",
                                               to = "ASCII//TRANSLIT"))
        }

        # This generates an Rmd Codebook and renders it to PDF (by default)
        # Note the autogenerated labelling during derivations will be lost if the
        # codebook is saved at this stage
        makeCodebook(d,
            file = file.path(PSYTOOLS_DATA_DICTIONARY_DIR, paste0(unlist(strsplit(filepath, '-'))[2], ".Rmd")),
            render = TRUE,
            openResult = FALSE,
            codebook = TRUE,
            replace = TRUE)
    }
    gc()
    options(datatable.print.nrows = 100)
}


process <- function(psc2_dir, processed_dir) {
    # Iterate over exported CSV Psytools files
    for (filename in list.files(psc2_dir)) {
        # The name of the questionnaire is based on the CSV file name
        name <- file_path_sans_ext(filename)

        # Read each exported CSV Psytools file into a data frame
        filepath <- file.path(psc2_dir, filename)
        COL_CLASSES = c(
            User.code = "character",
            Block = "character",
            Trial = "character",
            Response.time..ms. = "numeric")
        d <- read.csv(filepath, colClasses = COL_CLASSES, stringsAsFactors = FALSE)

        # Discard incomplete trials
        d <- subset(d, Completed == "t")
        # Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
        d <- subset(d, !grepl("Demo|MOCK|NPPILOT|TEST", User.code, ignore.case = TRUE))

        # Skip files without data - they cannot be rotated!
        if (nrow(d) < 2) {
            cat(name, ": skipping file without data.", sep = "", fill = TRUE)
            next
        }

        # Apply c-VEDA Custom Missings
        d <- applyCvedaCustomMissings(d)

        # Select the first or last iteration
        #  Currently using first complete iteration for cognitive tasks
        #  as well as KIRBY and SOCRATIS (completion is filtered above)
        #  and last complete iteration for all other questionnaires.
        filepath <- file.path(processed_dir, filename)
        if (grepl("SST|BART|ERT|TMT|WCST|_DS-|CORSI|MID|KIRBY|SOCRATIS", name)) {
            if (!grepl("KIRBY|SOCRATIS", name)) {
                selectIterationAndSave(d, min, gsub(".csv", "-RAW.csv", filepath))
            }
            if (grepl("SST", name)) {
                d <- deriveData(d, name)
                selectIterationAndSave(d[d$TaskVersion == "IMAGEN", ], min,
                  gsub(".csv", "-IMAGEN.csv", filepath))
                selectIterationAndSave(d[d$TaskVersion == "MARS", ], min,
                  gsub(".csv", "-MARS.csv", filepath))
            } else {
                selectIterationAndSave(deriveData(d, name), min, filepath)
            }
        } else {
            selectIterationAndSave(deriveData(d, name), max, filepath)
        }

        # Try to avoid out-of-memory condition
        rm(d)
        gc()
    }
}


process(PSYTOOLS_PSC1_DIR, PSYTOOLS_DERIVED_DIR)
