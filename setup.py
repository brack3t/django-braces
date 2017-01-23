from setuptools import setup

import braces

setup(
    name="django-braces",
    version=braces.__version__,
    description="Reusable, generic mixins for Django",
    long_description="Mixins to add easy functionality to Django class-based views, forms, and models.",
    keywords="django, views, forms, mixins",
    author="Kenneth Love <kenneth@brack3t.com>, Chris Jones <chris@brack3t.com>",
    author_email="devs@brack3t.com",
    url="https://github.com/brack3t/django-braces/",
    license="BSD",
    packages=["braces"],
    zip_safe=False,
    include_package_data=True,
    classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "License :: OSI Approved :: BSD License",
        "Environment :: Web Environment",
        "Development Status :: 5 - Production/Stable",
        "Programming Language :: Python :: 2.6",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.3",
        "Programming Language :: Python :: 3.4",
        "Programming Language :: Python :: 3.5",
        "Framework :: Django",
        "Framework :: Django :: 1.5",
        "Framework :: Django :: 1.6",
        "Framework :: Django :: 1.7",
        "Framework :: Django :: 1.8",
        "Framework :: Django :: 1.9",
        "Framework :: Django :: 1.10"
    ],
)
