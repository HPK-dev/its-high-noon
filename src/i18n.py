import enum
import json
import logging
import os

LOGGER = logging.getLogger("i18n")


class Langs(enum.Enum):
    EN = 'en'
    ZH_TW = 'zh_tw'

    @classmethod
    def from_str(cls, lang: str, default=EN):
        lang = cls.try_from_str(lang)
        return lang if lang else default

    @classmethod
    def try_from_str(cls, lang: str):
        lang = lang.strip().lower()

        if lang is None:
            return cls.EN

        # handle special cases
        elif lang == 'zh-tw':
            return cls.ZH_TW

        for l in cls:
            if l.value == lang:
                return l

        return None


class Keys(enum.Enum):
    PROCESSING_ERROR = 'processing_error'
    SET_LANG = 'set_lang'
    MISSING_ARGS = 'missing_args'
    AVAILABLE_LANGS = 'available_langs'
    RAN_OUT_QUESTIONS = 'ran_out_questions'
    COUNTDOWN = 'countdown'

    CMD_HELP = 'cmd_help'
    CMD_TOGGLE_ENABLE = 'cmd_toggle_enable'
    CMD_TOGGLE_DISABLE = 'cmd_toggle_disable'
    CMD_UNKNOWN = 'cmd_unknown'
    CMD_SCREAM = 'cmd_scream'


class I18nManager:
    def __init__(self, locale_directory: str, default_locale: Langs):
        self.locale_directory = locale_directory
        self.translations = {}
        self.default_locale = default_locale
        self.load_translations()
        LOGGER.info(f'Loaded {len(self.translations)} languages translations')

    def load_translations(self):
        for filename in os.listdir(self.locale_directory):
            if filename.endswith('.json'):
                locale = filename.split('.')[0]
                with open(os.path.join(self.locale_directory, filename), 'r', encoding='utf-8') as file:
                    self.translations[locale] = json.load(file)

    def get(self, key: Keys, locale: Langs = Langs.EN) -> str:
        key = key.value
        return self.translations.get(locale.value, {}).get(key, key)
