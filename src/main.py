import ipaddress
import logging
from functools import wraps

from flask import render_template
from flask import request

from src.const import APP, SCHEDULER
from src.database import question
from src.database.limiter import rate_limited

LOGGER = logging.getLogger("APP")


def local_only(f):
    @wraps(f)
    def decorated_function(*args, **kwargs):
        # Get client IP
        client_ip = request.remote_addr

        # List of allowed IPs/ranges
        allowed_ips = [
            '127.0.0.1',  # localhost
            '::1',  # localhost IPv6
            '192.168.0.0/16',  # typical local network range
            '10.0.0.0/8',  # private network range
        ]

        # Check if client IP is in allowed ranges
        is_allowed = False
        try:
            client_ip_obj = ipaddress.ip_address(client_ip)
            for allowed_ip in allowed_ips:
                try:
                    if '/' in allowed_ip:
                        network = ipaddress.ip_network(allowed_ip)
                        if client_ip_obj in network:
                            is_allowed = True
                            break
                    elif client_ip == allowed_ip:
                        is_allowed = True
                        break
                except ValueError:
                    continue

        except ValueError:
            return "", 404

        if not is_allowed:
            return "", 404

        return f(*args, **kwargs)

    return decorated_function


## Register routers ###
@APP.route("/", methods=["GET"])
def hello_world():
    return "Hello world from server!", 200


@APP.route("/teapot", methods=["GET"])
def teapot():
    return render_template("teapot.html"), 418


from src import line

APP.route("/line_bot_webhook", methods=["POST"])(line.webhook.callback)


### Sensitive endpoints ###
@APP.route("/db/question/create", methods=["POST"])
@rate_limited
def new_question():
    with APP.app_context():

        data = request.get_json()
        LOGGER.trace(f"Parsed data: {data}")

        fields = ["subject", "description", "opts", "ans"]
        fields_data = []
        for f in fields:
            if f not in data:
                return f"{f}", 400
            fields_data.append(data[f])

        fields_data.append(data.get("explanation"))
        fields_data.append(data.get("details"))

        question.create(*fields_data)
        return "", 200


@APP.route("/send/question", methods=["GET"])
@local_only
@rate_limited
def send_question():
    line.daily.send_question()
    return "", 200


@APP.route("/send/answer", methods=["GET"])
@local_only
@rate_limited
def send_answer():
    line.daily.send_answer()
    return "", 200


@APP.route("/send/countdown", methods=["GET"])
@local_only
@rate_limited
def send_countdown():
    line.daily.send_countdown()
    return "", 200


#######################

## Register scheduler ##

line.daily.register()


########################


def run():
    SCHEDULER.start()
    APP.run(host="0.0.0.0", port=8000, debug=True)
