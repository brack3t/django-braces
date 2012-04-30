# Source and destination file names.
test_source = "simple.txt"
test_destination = "multistyle_path_rst_html4css1.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html4css1"

# Settings
# test for encoded attribute value:
settings_overrides['stylesheet'] = ''
settings_overrides['stylesheet_path'] = 'ham.css,path/to/spam.css'
settings_overrides['embed_stylesheet'] = 0
