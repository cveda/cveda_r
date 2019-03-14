#!/usr/bin/env Rscript

# Copyright (c) 2017-2019 CEA
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


PSYTOOLS_PSC2_DIR <- "/cveda/databank/RAW/PSC2/psytools"
PSYTOOLS_PROCESSED_DIR <- "/cveda/databank/processed/psytools"


escape <- function(x) {
    if (class(x) == "character") {
        # Escape double quotation marks by doubling them
        x <- gsub('"', '""', x)
        # Enclose in quotation marks strings with commas or quotation marks
        x <- gsub('^(.*[",].*$)', '"\\1"', x)
    }
    return (x)
}


derivation <- function(name) {
  switch(
    name,
    "cVEDA-cVEDA_SOCRATIS-BASIC_DIGEST" = deriveSOCRATIS,
    "cVEDA-cVEDA_SOCRATIS_FU1-BASIC_DIGEST" = deriveSOCRATIS,
    "cVEDA-cVEDA_SST-BASIC_DIGEST" = deriveSST,
    "cVEDA-cVEDA_SST_FU1-BASIC_DIGEST" = deriveSST,
    "cVEDA-cVEDA_KIRBY-BASIC_DIGEST" = deriveKIRBY,
    "cVEDA-cVEDA_KIRBY_FU1-BASIC_DIGEST" = deriveKIRBY,
    "cVEDA-cVEDA_BART-BASIC_DIGEST" = deriveBART,
    "cVEDA-cVEDA_BART_FU1-BASIC_DIGEST" = deriveBART,
    "cVEDA-cVEDA_ERT-BASIC_DIGEST" = deriveERT,
    "cVEDA-cVEDA_ERT_FU1-BASIC_DIGEST" = deriveERT,
    "cVEDA-cVEDA_MID-BASIC_DIGEST" = deriveMID,
    "cVEDA-cVEDA_TMT-TMT_DIGEST" = deriveTMT,
    "cVEDA-cVEDA_TMT_FU1-TMT_DIGEST" = deriveTMT,
    "cVEDA-cVEDA_WCST-BASIC_DIGEST" = deriveWCST,
    "cVEDA-cVEDA_WCST_FU1-BASIC_DIGEST" = deriveWCST,
    "cVEDA-cVEDA_CORSI-BASIC_DIGEST" = deriveCORSI,
    "cVEDA-cVEDA_CORSI_FU1-BASIC_DIGEST" = deriveCORSI,
    "cVEDA-cVEDA_DS-BASIC_DIGEST" = deriveDS,
    "cVEDA-cVEDA_DS_FU1-BASIC_DIGEST" = deriveDS,
    "cVEDA-cVEDA_APQ_CHILD-BASIC_DIGEST" = deriveAPQ,
    "cVEDA-cVEDA_APQ_PARENT-BASIC_DIGEST" = deriveAPQ,
    "cVEDA-cVEDA_FHQ-BASIC_DIGEST" = rotateQuestionnairePreserveBlock,
    "cVEDA-cVEDA_FHQ_FU1-BASIC_DIGEST" = rotateQuestionnairePreserveBlock,
    "cVEDA-cVEDA_BIG5-BASIC_DIGEST" = deriveBIG5,
    "cVEDA-cVEDA_ASSIST-BASIC_DIGEST" = deriveASSIST,
    "cVEDA-cVEDA_ASSIST_FU1-BASIC_DIGEST" = deriveASSIST,
    "cVEDA-cVEDA_ASSIST_M_FU1-BASIC_DIGEST" = deriveASSIST,
    "cVEDA-cVEDA_ASSIST_F_FU1-BASIC_DIGEST" = deriveASSIST,
    "cVEDA-cVEDA_ACEIQ-BASIC_DIGEST" = deriveCvedaACEIQ,
    "cVEDA-cVEDA_ACEIQ_FU1-BASIC_DIGEST" = deriveCvedaACEIQ,
    "cVEDA-cVEDA_IFVCS-BASIC_DIGEST" = deriveIFVCS,
    "cVEDA-cVEDA_ANTHROPOMETRY-BASIC_DIGEST" = deriveCvedaAnthropometry,
    "cVEDA-cVEDA_ANTHROPOMETRY_FU1-BASIC_DIGEST" = deriveCvedaAnthropometry,
    "cVEDA-cVEDA_PBI-BASIC_DIGEST" = derivePBI,
    "cVEDA-cVEDA_PDS-BASIC_DIGEST" = deriveCvedaPDS,
    "cVEDA-cVEDA_PDS_FU1-BASIC_DIGEST" = deriveCvedaPDS,
    "cVEDA-cVEDA_SDQ_ADULT-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_ADULT_FU1-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_CHILD-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_CHILD_FU1-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_PARENT-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_PARENT_FU1-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SDQ_PARENT_SELF_FU1-BASIC_DIGEST" = deriveSDQ,
    "cVEDA-cVEDA_SCQ-BASIC_DIGEST" = deriveSCQ,
    "cVEDA-cVEDA_SCQ_FU1-BASIC_DIGEST" = deriveSCQ,
    "cVEDA-cVEDA_SDIM-BASIC_DIGEST" = deriveCvedaSDIM,
    "cVEDA-cVEDA_SDIM_FU1-BASIC_DIGEST" = deriveCvedaSDIM,
    rotateQuestionnaire
    )  # default fits all other questionnaires
}

process <- function(psc2_dir, processed_dir) {
    # Iterate over exported CSV Psytools files
    for (filename in list.files(psc2_dir)) {
        # The name of the questionnaire is based on the CSV file name
        name <- file_path_sans_ext(filename)

        # Read each exported CSV Psytools file into a data frame
        filepath <- file.path(psc2_dir, filename)
        COL_CLASSES = c(
            "User.code"="character",
            "Block"="character",
            "Trial"="character",
            "Response.time..ms."="numeric")
        d <- read.csv(filepath, colClasses=COL_CLASSES, stringsAsFactors=FALSE)

        # Discard uncomplete trials
        d <- subset(d, Completed=='t')
        # Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
        d <- subset(d, !grepl("Demo|MOCK|NPPILOT|TEST", User.code, ignore.case=TRUE))

        # Skip files without data - they cannot be rotated!
        if (nrow(d) < 2) {
            cat(name, ": skipping file without data.", sep="", fill=TRUE)
            next
        }
        
        # Apply cVeda Custom Missings
        d <- applyCvedaCustomMissings(d)
        
        # Apply relevant derivation function to each questionnaire
        derivation_function <- derivation(name)
        d <- derivation_function(d)
        
        # Extract "Age.band" from "User.code"
        d$Age.band <- substr(d$User.code, 14, 15)
        d$User.code <- substr(d$User.code, 1, 12)
        d <- d[c(1, ncol(d), 2:(ncol(d)-1))]

        # Remake iteration field if iterations exist under multiple age bands for the same PSC
        d<-d[order(d$User.code, d$Completed.Timestamp),]
        d$Iteration<-unlist(tapply(d$User.code, d$User.code, seq_along))

        # Select the first or last iteration
        #  Currently using first for cognitive tasks (completion is filtered above)
        #  And last complete for questionnaires (everything else)
        if(grepl("MID|SOCRATIS|SST|BART|KIRBY|ERT|TMT|WCST|DS", name)) {
             iterationFunction<-min
        } else {
             iterationFunction<-max
        }
        d <- merge(d,
                   aggregate(Iteration ~ User.code,
                             iterationFunction,
                             data = d),
                   by = c("User.code", "Iteration"),
                   sort = FALSE)

        # Roll our own quoting method
        for (column in colnames(d)) {
            d[,column] <- escape(d[,column])
        }

        # Write data frame back to the processed CSV file
        filepath <- file.path(processed_dir, filename)
        columns <- sub("\\.ms\\.", "[ms]", colnames(d))  # Response time [ms]
        columns <- gsub("\\.", " ", columns)
        write.table(d, filepath, quote=FALSE, sep=",", na="",
                    row.names=FALSE, col.names=columns)

        # Try to avoid out-of-memory condition
        rm(d)
        gc()
    }
}


process(PSYTOOLS_PSC2_DIR, PSYTOOLS_PROCESSED_DIR)
