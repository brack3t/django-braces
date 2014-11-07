import os
from django.conf import settings
from tests import settings as test_settings


def pytest_configure():
    os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'tests.settings')
    settings.configure(default_settings=test_settings)
