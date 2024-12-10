import logging
import uuid
from datetime import datetime
from typing import List

from linebot.v3.messaging import (
    ApiClient,
    TextMessage,
    MessagingApi, MulticastRequest
)
from pydantic import StrictStr

from src.const import SCHEDULER, I18N
from src.database import question, user
from src.database.question import Question
from src.i18n import Keys
from src.line import CONFIGURATION

LOGGER = logging.getLogger(__name__)

TODAY_QUESTION: Question | None = None


def make_question() -> str | None:
    LOGGER.info("Making question")
    global TODAY_QUESTION
    TODAY_QUESTION = question.random_one(True)
    return TODAY_QUESTION.make_question() if TODAY_QUESTION else None


def make_answer() -> str | None:
    LOGGER.info("Making answer")
    global TODAY_QUESTION
    return TODAY_QUESTION.make_answer() if TODAY_QUESTION else None


def countdown() -> int:
    gsat_data = datetime(2025, 1, 18)
    today = datetime.now()
    return (gsat_data - today).days + 1


def send_msgs(msgs, users: List[str]):
    to = [users[i:i + 500] for i in range(0, len(users), 500)]

    LOGGER.info(f"Sending messages to {len(users)} users")

    requests = ([
        MulticastRequest(
            messages=msgs,
            to=t,
            notificationDisabled=None,
            customAggregationUnits=None
        ) for t in to])

    tasks = []

    with ApiClient(CONFIGURATION) as api_client:
        line_bot_api = MessagingApi(api_client)
        for req in requests:
            thread = line_bot_api.multicast(req, x_line_retry_key=StrictStr(str(uuid.uuid4())), async_req=True)
            tasks.append(thread)

    for task in tasks:
        result = task.get()
        LOGGER.debug(result)


def send_question():
    LOGGER.debug("Send question")

    targets = user.get_enabled()
    for (lang, users) in targets.items():
        question_text = make_question() or I18N.get(Keys.RAN_OUT_QUESTIONS, lang)

        send_msgs([TextMessage(
            text=StrictStr(question_text),
            emojis=None,
            quoteToken=None,
            quickReply=None
        )], users)


def send_answer():
    LOGGER.debug("Send answer")

    targets = user.get_enabled()
    for (lang, users) in targets.items():
        answer_text = make_answer() or I18N.get(Keys.RAN_OUT_QUESTIONS, lang)

        send_msgs([TextMessage(
            text=StrictStr(answer_text),
            emojis=None,
            quoteToken=None,
            quickReply=None
        )], users)


def send_countdown():
    LOGGER.debug("Send countdown")

    targets = user.get_all()

    for (lang, users) in targets.items():
        text = I18N.get(Keys.COUNTDOWN, lang).format(countdown())

        send_msgs([TextMessage(
            text=StrictStr(text),
            emojis=None,
            quoteToken=None,
            quickReply=None
        )], users)


def register():
    SCHEDULER.register(send_countdown, "00:00", True)
    SCHEDULER.register(send_question, "00:00", True)
    SCHEDULER.register(send_answer, "02:00", True)
