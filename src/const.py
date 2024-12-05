import os
from logging import getLogger
from typing import TypeVar

from dotenv import load_dotenv
from flask import Flask
from flask.logging import default_handler

T = TypeVar("T")

NAME = "It's high noon"
load_dotenv()

from src.logger import setup_logger

setup_logger()


def get_env(key: str) -> str | None:
    return os.getenv(key)


def get_env_or_exit(key: str) -> str:
    r = os.getenv(key)
    if r is None:
        raise RuntimeError(f"Environment variable {key} not set")
    else:
        return r


def get_env_or_default(key: str, default: T) -> T:
    r = os.getenv(key)
    if r is None:
        return default
    else:
        return r


LINE_CHANNEL_SECRET = get_env_or_exit("LINE_CHANNEL_SECRET")
LINE_CHANNEL_ACCESS_TOKEN = get_env_or_exit("LINE_CHANNEL_ACCESS_TOKEN")

DB_NAME = get_env_or_exit("DB_NAME")
DB_PORT = get_env_or_default("DB_PORT", 5432)
DB_USER = get_env_or_exit("DB_USER")
DB_HOST = get_env_or_default("DB_HOST", "localhost")
DB_PASSWORD = get_env_or_exit("DB_PASSWORD")

from src.database.pool import ConnectionPool

DATABASE = ConnectionPool(DB_NAME, DB_USER, DB_PASSWORD, DB_HOST, DB_PORT)

from src.database import user, question, limiter

user.init_db()
question.init_db()
limiter.init_db()

APP = Flask(NAME)
getLogger(APP.name).removeHandler(default_handler)

from src import scheduler

SCHEDULER = scheduler.Scheduler()

from src.i18n import I18nManager

I18N = I18nManager("./locales", "en")
