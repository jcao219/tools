exception File_not_found of string

let cEXPECTED_VERSION = "4.01.0"
let cEMAIL_SUBJECT = "[CS3110 test harness] compile error"
let cEMAIL_ADMINS = "_email_bcc.txt"

let cCMS_DIR = "./_cms"
let cCMS_FNAME = Format.sprintf "%s/spreadsheet.csv" cCMS_DIR
let cCMS_FINAL = Format.sprintf "%s/CS3110_GRADES_TABLE.csv" cCMS_DIR
let cDEPEND_FILE = "./.depend"
let cDIFF_RESULTS = "_diff_results.csv"
let cEMAIL_DIR = "./_email"
let cLIB_FILE = "./.libs"
let cNOCOMPILE_DIR = "./_nocompile"
let cOPAM_PACKAGES_FILE = "./.opam_packages"
let cOUTPUT_DIR = "./_output"
let cSMOKE_TARGETS = "./smoke_test"
(* std_opam_packages may NOT be empty! Need pa_ounit, at least, to compile *)
let cSTD_OPAM_PACKAGES = ["pa_ounit.syntax"; "oUnit"; "pa_ounit"; "qcheck"]
let cTESTS_DIR = "./tests"

let cTEST_OUTPUT = "inline_tests.log"
let cFAIL_OUTPUT = "inline_test_failures.log"
