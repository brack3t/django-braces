# Source and destination file names.
test_source = "data/math.txt"
test_destination = "math_output_html.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Extra setting

settings_overrides['math_output'] = 'HTML'
settings_overrides['stylesheet_path'] = (
    '../docutils/writers/html4css1/html4css1.css, '
    '../docutils/writers/html4css1/math.css ')
    
