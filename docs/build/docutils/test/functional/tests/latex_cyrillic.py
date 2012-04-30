# Source and destination file names.
test_source = "cyrillic.txt"
test_destination = "cyrillic.tex"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "latex"

# Extra setting we need

settings_overrides['hyperref_options'] = 'unicode=true'
settings_overrides['font_encoding'] = 'T1,T2A'
settings_overrides['stylesheet'] = 'cmlgc'
settings_overrides['language_code'] = 'ru'
