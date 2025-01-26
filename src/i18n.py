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
    CMD_ABOUT = 'cmd_about'
    
    JOKE_REPLY = 'joke_reply'
    JOKE_RESPONSE = 'joke_response'

    EAT_REPLY = 'eat_reply'
    EAT_RESPONSE = 'eat_response'
    EAT_RESPONSE_TEMPLATE = 'eat_response_template'

    ONLINE_REPLY = 'online_reply'
    ONLINE_RESPONSE = 'online_response'

    CHEERS_REPLY = 'cheers_reply'
    CHEERS_RESPONSE = 'cheers_response'

    HAHA_REPLY = 'haha_reply'
    HAHA_RESPONSE = 'haha_response'

    WEATHER_REPLY = 'weather_reply'
    WEATHER_RESPONSE = 'weather_response'
    WEATHER_DESC = 'weather_desc'
    ACTIVITY = 'activity'

    GREETINGS_REPLY = 'greetings_reply'
    GREETINGS_RESPONSE = 'greetings_response'

    DECISION_REPLY = 'decision_reply'
    DECISION_RESPONSE = 'decision_response'

    TRIVIA_REPLY = 'trivia_reply'
    TRIVIA_RESPONSE = 'trivia_response'

    ENCOURAGEMENT_REPLY = 'encouragement_reply'
    ENCOURAGEMENT_RESPONSE = 'encouragement_response'

    QUOTE_REPLY = 'quote_reply'
    QUOTE_RESPONSE = 'quote_response'

    CALCULATE_REPLY = "calculate_reply"
    CALCULATE_RESPONSE = 'calculate_response'
    CALCULATE_NO_RESPONSE = 'calculate_no_response'
    CALCULATE_WITH_SNARKY_RESPONSE = 'calculate_with_snarky_response'

    MYGO_REPLY = 'mygo_reply'
    MYGO_RESPONSE = 'mygo_response'

    CHAT_REPLY = 'chat_reply'
    CHAT_RESPONSE_TEMPLATE = 'chat_response_template'
    CHAT_RESPONSE_FEEL_TEMPLATE = 'chat_response_feel_template'
    CHAT_RESPONSE_FEEL_COMPONENT1 = 'chat_response_feel_component1'
    CHAT_RESPONSE_FEEL_COMPONENT2 = 'chat_response_feel_component2'
    CHAT_RESPONSE_DISCOVERY = 'chat_response_discovery'
    CHAT_RESPONSE_CONCLUSION = 'chat_response_conclusion'
    CHAT_RESPONSE_ACTION = 'chat_response_action'

    NONSENSE_REPLY = 'nonsense_reply'
    NONSENSE_PARAGRAPH_TEMPLATE= 'nonsense_paragraph_template'
    NONSENSE_SENTENCE1_TEMPLATE = 'nonsense_sentence1_template'
    NONSENSE_SENTENCE2_TEMPLATE = 'nonsense_sentence2_template'
    NONSENSE_SENTENCE3_TEMPLATE = 'nonsense_sentence3_template'
    NONSENSE_SENTENCE4_TEMPLATE = 'nonsense_sentence4_template'
    NONSENSE_SENTENCE5_TEMPLATE = 'nonsense_sentence5_template'
    NONSENSE_SENTENCE6_TEMPLATE = 'nonsense_sentence6_template'
    NONSENSE_COMPONENT1 = 'nonsense_component1'
    NONSENSE_COMPONENT2 = 'nonsense_component2'
    NONSENSE_COMPONENT3 = 'nonsense_component3'
    NONSENSE_COMPONENT4 = 'nonsense_component4'

    CHOOSE_REPLY = 'choose_reply'
    CHOOSE_RESPONSE = 'choose_response'

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
