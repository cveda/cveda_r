#!/usr/bin/env Rscript

# Copyright (c) 2017-2018 CEA
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

        # Add an index to preserve order (to simplify eyeballing)
        d$rowIndex <- seq_len(nrow(d))

        # Apply relevant derivation function to each questionnaire
        if (name == "cVEDA-cVEDA_SOCRATIS-BASIC_DIGEST") {
            d <- deriveSOCRATIS(d)
        } else if (name == "cVEDA-cVEDA_SST-BASIC_DIGEST") {
            d <- deriveSST(d)
        } else if (name == "cVEDA-cVEDA_KIRBY-BASIC_DIGEST") {
            d <- deriveKIRBY(d)
        } else if (name == "cVEDA-cVEDA_BART-BASIC_DIGEST") {
            d <- deriveBART(d)
        } else if (name == "cVEDA-cVEDA_ERT-BASIC_DIGEST") {
            d <- deriveERT(d)
        } else if (name == "cVEDA-cVEDA_MID-BASIC_DIGEST") {
            d <- deriveMID(d)
        } else if (name == "cVEDA-cVEDA_TMT-TMT_DIGEST") {
            d <- deriveTMT(d)
        } else if (name == "cVEDA-cVEDA_WCST-BASIC_DIGEST") {
            d <- deriveWCST(d)
        } else if (name == "cVEDA-cVEDA_CORSI-BASIC_DIGEST") {
            d <- deriveCORSI(d)
        } else if (name == "cVEDA-cVEDA_DS-BASIC_DIGEST") {
            d <- deriveDS(d)
        } else if (name == "cVEDA-cVEDA_FHQ-BASIC_DIGEST" || name == "cVEDA-cVEDA_FHQ_FU1-BASIC_DIGEST") {
            d <- rotateQuestionnairePreserveBlock(d)
        } else {
            x <- tryCatch(
                d <- rotateQuestionnaire(d),
                error = function(e) {
                    cat(name, ":", conditionMessage(e), "\n")
                }
            )
            if (class(x) == "NULL") {
                next
            }
        }

        # Extract "Age.band" from "User.code"
        d$Age.band <- as.numeric(substr(d$User.code, 15, 15))
        d$User.code <- substr(d$User.code, 1, 12)
        d <- d[c(1, ncol(d), 2:(ncol(d)-1))]

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
