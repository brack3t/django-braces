# Source and destination file names.
test_source = "data/math.txt"
test_destination = "math_output_latex.html"

# Keyword parameters passed to publish_file.
reader_name = "standalone"
parser_name = "rst"
writer_name = "html"

# Extra setting

settings_overrides['math_output'] = 'latex'
    
