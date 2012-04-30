# Source and destination file names.
test_source = "cyrillic.txt"
test_destination = "xetex-cyrillic.tex"

# Keyword parameters passed to publish_file.
writer_name = "xetex"

# Settings
settings_overrides['language_code'] = 'ru'
# Override the automatic addition of "unicode" option for "russian"
# language to work around a problem with cyrillic in PDF-bookmarks in
# hyperref versions < v6.79g 2009/11/20 
settings_overrides['hyperref_options'] = 'unicode=false'
