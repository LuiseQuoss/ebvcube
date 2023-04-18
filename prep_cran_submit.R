#BEFORE SUBMITTING TO CRAN

#build readme and manual
devtools::build_manual()
devtools::build_readme()

# Run tests and examples
devtools::test()
devtools::run_examples()

# Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))

# Check content
#install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
# checkhelper::find_missing_tags()
# # _Check that you let the house clean after the check, examples and tests
# all_files_remaining <- checkhelper::check_clean_userspace()
# all_files_remaining

# Check spelling
# usethis::use_spell_check()
# spelling::spell_check_package()


# Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
# urlchecker::url_update()

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# check on other distributions
# _rhub
cran_prep <- check_for_cran()
cran_prep$cran_summary()
# devtools::check_rhub()
# rhub::check_on_windows(check_args = "--force-multiarch")
# rhub::check_on_solaris()
# # _win devel
# devtools::check_win_devel()

# # Check reverse dependencies
# # remotes::install_github("r-lib/revdepcheck")
# install.packages('revdepcheck', repos = 'https://r-lib.r-universe.dev')
# usethis::use_git_ignore("revdep/")
# usethis::use_build_ignore("revdep/")
#
# devtools::revdep()
# library(revdepcheck)
# In another session
# id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
# rstudioapi::terminalKill(id)
# # See outputs
# revdep_details(revdep = "pkg")
# revdep_summary()                 # table of results by package
# revdep_report() # in revdep/
# # Clean up when on CRAN
# revdep_reset()


# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())


#MANUALLY
#update date in DESCRIPTION
#upgraded version number correctly?
#news uptodate?

# Verify you're ready for release, and release
devtools::release()
