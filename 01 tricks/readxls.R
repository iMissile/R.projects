library(tidyverse)
library(stringr)
library(readxl)

fname <- "col_test.xlsx"
ctypes <- readxl:::xlsx_col_types(fname) # не очень красиво, работает для xlsx
length(ctypes) # 5
ncol(read_excel(fname)) # 2

ctypes <- readxl:::xls_col_types("col_test.xls", na="") # не очень красиво, работает для xlsx
length(ctypes) # 5
ncol(read_excel("col_test.xls")) # 2

# ===============================================
  
cnames <- str_c("grp_", seq_along(ctypes))
raw <- read_excel(fname,
                  col_types = ctypes,
                  col_names = cnames)

# =====================
raw <- read_excel(fname)
ncol(raw)
ctypes <- rep("text", ncol(raw))
cnames <- str_c("grp_", seq_along(ctypes))
raw <- read_excel(fname,
                  col_types = ctypes,
                  col_names = cnames)

