import dataclasses
import logging
import random
import re
from typing import Optional

from flask import request, abort
from linebot.v3.exceptions import InvalidSignatureError
from linebot.v3.messaging import ApiClient, MessagingApi, ReplyMessageRequest, TextMessage, ShowLoadingAnimationRequest
from linebot.v3.webhooks import MessageEvent, TextMessageContent, UserSource, GroupSource
from pydantic import StrictStr, StrictBool

from src.const import I18N
from src.database import user
from src.i18n import Keys, Langs
from src.line import HANDLER, CONFIGURATION
from src.line.cmd import UnknownCommandError, MissingArgumentsError, NoCommandError, CMD
from src.line.calculate import calculate
from src.line.chat_bot_response import chatbot_response

LOGGER = logging.getLogger("line-webhook")


def callback():
    """Handle LINE webhook callbacks."""
    try:
        signature = request.headers["X-Line-Signature"]
        body = request.get_data(as_text=True)

        LOGGER.info("Received webhook: length=%d", len(body))

        HANDLER.handle(body, signature)
        return "OK", 200

    except KeyError:
        LOGGER.error("Missing X-Line-Signature header")
        abort(400, description="Missing X-Line-Signature header")

    except InvalidSignatureError:
        LOGGER.error("Invalid signature")
        abort(401, description="Invalid signature")

    except Exception as e:
        LOGGER.error("Unexpected error: %s", str(e), exc_info=True)
        abort(500, description="Internal server error")


def send_reply(event: MessageEvent, reply_text: str, quote_token=Optional[str]) -> None:
    """Send a reply message to LINE."""

    with ApiClient(CONFIGURATION) as api_client:
        line_bot_api = MessagingApi(api_client)
        line_bot_api.reply_message(
            ReplyMessageRequest(
                replyToken=event.reply_token,
                notificationDisabled=StrictBool(False),
                messages=[
                    TextMessage(
                        text=StrictStr(reply_text),
                        quickReply=None,
                        quoteToken=StrictStr(quote_token) if quote_token else None,
                    )
                ],
            ),
            async_req=True
        )


def loading_animate(chat_id: str) -> None:
    """Send a loading animation to LINE."""
    with ApiClient(CONFIGURATION) as api_client:
        line_bot_api = MessagingApi(api_client)
        line_bot_api.show_loading_animation(show_loading_animation_request=ShowLoadingAnimationRequest(
            loadingSeconds=20,
            chatId=StrictStr(chat_id)
        ))


#########################################################

@dataclasses.dataclass
class ProcessContext:
    event: MessageEvent
    user_id: Optional[str]
    lang: Langs
    quote_token: Optional[str]


@HANDLER.add(MessageEvent, message=TextMessageContent)
def message(event: MessageEvent) -> None:
    """Handle incoming text messages."""
    try:
        LOGGER.debug("Received request: %s", event)

        user_id = None
        if isinstance(event.source, UserSource) or isinstance(event.source, GroupSource):
            user_id = event.source.user_id
            loading_animate(user_id)
            user.create(user_id)

        ctx = ProcessContext(
            event,
            user_id,
            Langs.from_str(user.get_lang(user_id)),
            None
        )
        LOGGER.debug(
            "Context: user_id=%s, lang=%s",
            ctx.user_id, ctx.lang
        )

        reply = process_message(ctx)

        if not reply:
            return

        LOGGER.debug("Sent reply: %s", reply)
        send_reply(event, reply, ctx.quote_token)

    except Exception as e:
        LOGGER.error("Error processing message: %s", str(e), exc_info=True)
        # Send a generic error message to user
        send_reply(event, I18N.get(Keys.PROCESSING_ERROR))

def process_and_select(input_string):
    keywords = ["幫我選", "幫選", "選一下", "選哪個", "選一個", "隨機一個", "隨機", "抽取", "抽獎", "抽一個"]
    for keyword in keywords:
        input_string = input_string.replace(keyword, "")
    parts = input_string.split()
    if len(parts) == 1:
        parts = parts[0].split("\n")
    if len(parts) == 1:
        parts = parts[0].split(",")
    if len(parts) == 1:
        return "error_404"
    return random.choice([part.strip() for part in parts if part.strip() != ""])

def process_message(ctx: ProcessContext) -> str | None:
    """Process incoming message and generate reply."""
    # A text event
    if isinstance(ctx.event.message, TextMessageContent):
        text = ctx.event.message.text.strip()
        ctx.quote_token = ctx.event.message.quote_token
        LOGGER.debug(
            "Received text message: text=%s, user_id=%s, lang=%s",
            text, ctx.user_id, ctx.lang
        )

        if text.startswith("/"):
            try:
                return CMD.parse_and_execute(text[1:], ctx)
            except NoCommandError:
                return "owob"
            except UnknownCommandError:
                return I18N.get(Keys.CMD_UNKNOWN, ctx.lang)
            except MissingArgumentsError as e:
                return I18N.get(Keys.MISSING_ARGS, ctx.lang).format(str(e.missing_args))

        elif ("ouo" in text.lower()
              or "owo" in text.lower()
              or "uwu" in text.lower()):
            return "Ciallo (∠·ω )⌒★"
        elif any(s in text for s in I18N.get(Keys.EAT_REPLY, ctx.lang)):
            return random.choice(I18N.get(Keys.EAT_RESPONSE_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.EAT_RESPONSE, ctx.lang)))
        elif text == "+1":
            return "+1"
        elif any(s in text for s in I18N.get(Keys.JOKE_REPLY, ctx.lang)):
            return random.choice(I18N.get(Keys.JOKE_RESPONSE, ctx.lang))
        elif text == "666":
            return "666"
        elif any(s in text for s in I18N.get(Keys.ONLINE_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.ONLINE_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.CHEERS_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.CHEERS_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.HAHA_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.HAHA_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.WEATHER_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.WEATHER_RESPONSE, ctx.lang)).format(
                random.choice(I18N.get(Keys.WEATHER_DESC, ctx.lang)),
                random.choice(I18N.get(Keys.ACTIVITY, ctx.lang))
            )
        elif any(s in text for s in I18N.get(Keys.GREETINGS_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.GREETINGS_RESPONSE, ctx.lang))

        elif any(s in text for s in I18N.get(Keys.DECISION_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.DECISION_RESPONSE, ctx.lang))

        elif any(s in text for s in I18N.get(Keys.TRIVIA_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.TRIVIA_RESPONSE, ctx.lang))

        elif any(s in text for s in I18N.get(Keys.ENCOURAGEMENT_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.ENCOURAGEMENT_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.QUOTE_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.QUOTE_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.MYGO_REPLY, ctx.lang)): 
            return random.choice(I18N.get(Keys.MYGO_RESPONSE, ctx.lang))
        elif any(s in text for s in I18N.get(Keys.CHAT_REPLY, ctx.lang)):
            feel_template = random.choice(I18N.get(Keys.CHAT_RESPONSE_FEEL_TEMPLATE, ctx.lang))
            feel_component1 = random.choice(I18N.get(Keys.CHAT_RESPONSE_FEEL_COMPONENT1, ctx.lang))
            feel_component2 = random.choice(I18N.get(Keys.CHAT_RESPONSE_FEEL_COMPONENT2, ctx.lang))
            return random.choice(I18N.get(Keys.CHAT_RESPONSE_TEMPLATE, ctx.lang)).format(
                feel_template.format(feel_component1, feel_component2),
                random.choice(I18N.get(Keys.CHAT_RESPONSE_DISCOVERY, ctx.lang)),
                random.choice(I18N.get(Keys.CHAT_RESPONSE_CONCLUSION, ctx.lang)),
                random.choice(I18N.get(Keys.CHAT_RESPONSE_ACTION, ctx.lang))
            )
        elif any(s in text for s in I18N.get(Keys.CALCULATE_REPLY, ctx.lang)): 
            if random.randint(1, 2) <= 1:
                return random.choice(I18N.get(Keys.CALCULATE_NO_RESPONSE, ctx.lang))
            else:
                match = re.search(r"[0-9+\-*/%^]+", text)
                calculation_formula = match.group(0) if match else ""
                return random.choice(I18N.get(Keys.CALCULATE_WITH_SNARKY_RESPONSE, ctx.lang)).format(calculate(calculation_formula))
        elif any(s in text for s in I18N.get(Keys.NONSENSE_REPLY, ctx.lang)):
            sentence1 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE1_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT1, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT2, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT3, ctx.lang))
            )
            sentence2 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE2_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT3, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT4, ctx.lang))
            )
            sentence3 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE3_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT2, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT4, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT1, ctx.lang))
            )
            sentence4 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE4_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT4, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT2, ctx.lang))
            )
            sentence5 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE5_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT1, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT3, ctx.lang))
            )
            sentence6 = random.choice(I18N.get(Keys.NONSENSE_SENTENCE6_TEMPLATE, ctx.lang)).format(
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT2, ctx.lang)),
                random.choice(I18N.get(Keys.NONSENSE_COMPONENT4, ctx.lang))
            )
            return I18N.get(Keys.NONSENSE_PARAGRAPH_TEMPLATE, ctx.lang).format(
                sentence1, sentence2, sentence3, sentence4, sentence5, sentence6
            )
        elif any(s in text for s in I18N.get(Keys.CHOOSE_REPLY, ctx.lang)):
            anser = process_and_select(text)
            if anser == "error_404":
                pass
            else:
                return random.choice(I18N.get(Keys.CHOOSE_RESPONSE, ctx.lang)).format(anser)
        elif any(s in text for s in I18N.get(Keys.CHAT_BOT_REPLY, ctx.lang)): 
            return chatbot_response(text)
    return None
