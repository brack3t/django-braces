import os
import re
from setuptools import setup

NAME = "braces"


def _add_default(m):
    """Add on a default"""
    attr_name, attr_value = m.groups()
    return ((attr_name, attr_value.strip("\"'")),)


def parse_dist_meta():
    """Extract metadata information from ``$dist/__init__.py``."""

    re_meta = re.compile(r"__(\w+?)__\s*=\s*(.*)")
    re_doc = re.compile(r'^"""(.+?)"""')
    here = os.path.abspath(os.path.dirname(__file__))
    with open(os.path.join(here, NAME, "__init__.py")) as meta_fh:
        distmeta = {}
        for line in meta_fh:
            if line.strip() == "# -eof meta-":
                break
            match = re_meta.match(line.strip())
            if match:
                distmeta.update(_add_default(match))
        return distmeta


meta = parse_dist_meta()

setup(
    name="django-braces",
    version=meta["version"],
    description="Reusable, generic mixins for Django",
    long_description="Mixins to add easy functionality to Django class-based views, forms, and models.",
    keywords="django, views, forms, mixins",
    author="Kenneth Love <kenneth@brack3t.com>, Chris Jones <chris@brack3t.com>",
    author_email="devs@brack3t.com",
    url="https://github.com/brack3t/django-braces/",
    license="BSD",
    packages=["braces", "braces.views"],
    zip_safe=False,
    include_package_data=True,
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Development Status :: 6 - Mature",
        "Environment :: Web Environment",
        "Framework :: Django",
        "Framework :: Django :: 2.2",
        "Framework :: Django :: 3.2",
        "Framework :: Django :: 4.0",
        "Framework :: Django :: 4.1",
        "Framework :: Django :: 4.2",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: BSD License",
        "Natural Language :: English",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3 :: Only",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Programming Language :: Python :: 3.13",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
    install_requires=["Django>=2.2"],
)
