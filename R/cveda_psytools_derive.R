#!/usr/bin/env Rscript

# Copyright (c) 2017 CEA
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


PSYTOOLS_PSC2_DIR <- "/cveda/databank/RAW/PSC1/psytools"
PSYTOOLS_PROCESSED_DIR <- "/cveda/databank/processed/psytools"

BOGUS <- list("cVEDA-cVEDA_MID-BASIC_DIGEST",
              "cVEDA-cVEDA_SST-BASIC_DIGEST",
              "cVEDA-cVEDA_WCST-BASIC_DIGEST")


quote <- function(x) {
    if (class(x) == "character") {
        # Escape double quotation marks by doubling them
        x <- gsub('"', '""', x)
        # Enclose in quotation marks strings with commas or quotation marks
        x <- gsub('^(.*[",].*$)', '"\\1"', x)
    }
    return (x)
}


# Iterate over exported CSV Psytools files
for (filename in list.files(PSYTOOLS_PSC2_DIR)) {
    # The name of the questionnaire is based on the CSV file name
    name <- file_path_sans_ext(filename)

    if (name %in% BOGUS) {
        next
    }

    # Read each exported CSV Psytools file into a data frame
    filepath <- file.path(PSYTOOLS_PSC2_DIR, filename)
    COL_CLASSES = c(
        "User.code"="character",
        "Block"="character",
        "Trial"="character",
        "Response.time..ms."="numeric")
    df <- read.csv(filepath, colClasses=COL_CLASSES, stringsAsFactors=FALSE)

    # Discard uncomplete trials
    df <- subset(df, df$Completed=='t')
    # Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
    df <- subset(df, !grepl("Demo|MOCK|NPPILOT|TEST", User.code, ignore.case=TRUE))

    # Add an index to preserve order (to simplify eyeballing)
    df$rowIndex <- seq_len(nrow(df))

    # Apply relevant derivation function to each questionnaire
    if (name == "cVEDA-cVEDA_SOCRATIS-BASIC_DIGEST") {
        df <- deriveSOCRATIS(df)
    } else if (name == "cVEDA-cVEDA_SST-BASIC_DIGEST") {
        df <- deriveSST(df)
    } else if (name == "cVEDA-cVEDA_KIRBY-BASIC_DIGEST") {
        df <- deriveKIRBY(df)
    } else if (name == "cVEDA-cVEDA_BART-BASIC_DIGEST") {
        df <- deriveBART(df)
    } else if (name == "cVEDA-cVEDA_ERT-BASIC_DIGEST") {
        df <- deriveERT(df)
    } else if (name == "cVEDA-cVEDA_MID-BASIC_DIGEST") {
        print(name)
        df <- deriveMID(df)
        print("DONE")
    } else if (name == "cVEDA-cVEDA_TMT-TMT_DIGEST") {
        df <- deriveTMT(df)
    } else if (name == "cVEDA-cVEDA_WCST-BASIC_DIGEST") {
        print(name)
        df <- deriveWCST(df)
        print("DONE")
    } else if (name == "cVEDA-cVEDA_CORSI-BASIC_DIGEST") {
        df <- deriveCORSI(df)
    } else if (name == "cVEDA-cVEDA_DS-BASIC_DIGEST") {
        df <- deriveDS(df)
    } else {
        print(name)
        df <- rotateQuestionnaire(df)
        print("DONE")
    }

    # Extract "Age.band" from "User.code"
    df$Age.band <- as.numeric(substr(df$User.code, 15, 15))
    df$User.code <- substr(df$User.code, 1, 12)
    df <- df[c(1, ncol(df), 2:(ncol(df)-1))]

    # Roll our own quoting method
    for (column in colnames(df)) {
        df[,column] <- quote(df[,column])
    }

    # Write data frame back to the processed CSV file
    filepath <- file.path(PSYTOOLS_PROCESSED_DIR, filename)
    columns <- sub("\\.ms\\.", "[ms]", colnames(df))  # Response time [ms]
    columns <- gsub("\\.", " ", columns)
    write.table(df, filepath, quote=FALSE, sep=",", na="",
                row.names=FALSE, col.names=columns)
}
