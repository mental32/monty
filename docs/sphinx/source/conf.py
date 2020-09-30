# https://www.sphinx-doc.org/en/master/usage/configuration.html
# -- Path setup --------------------------------------------------------------

import os
import sys
sys.path.insert(0, os.path.abspath('../../..'))

# -- Project information -----------------------------------------------------

project = 'monty'
copyright = '2020, mental'
author = 'mental'
release = '0.0.0'

# -- General configuration ---------------------------------------------------

extensions = ["sphinx.ext.napoleon"]
templates_path = ['_templates']
exclude_patterns = []

# -- Options for HTML output -------------------------------------------------

html_theme = 'alabaster'
html_static_path = ['_static']
