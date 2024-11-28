import dataclasses
import logging
import random
from typing import Callable, List, Optional

from src.const import I18N
from src.database import user
from src.i18n import Keys, Langs

LOGGER = logging.getLogger("line-webhook-cmd")


class NoCommandError(Exception):
    pass


class UnknownCommandError(Exception):
    cmd_name: str

    def __init__(self, cmd_name):
        self.cmd_name = cmd_name


class MissingArgumentsError(Exception):
    cmd_name: str
    missing_args: list[str]

    def __init__(self, cmd_name, missing_args):
        self.cmd_name = cmd_name
        self.missing_args = missing_args


@dataclasses.dataclass
class Command:
    name: str
    func: Callable
    required_args: List[str]
    optional_args: Optional[List[str]]


class CommandBuilder:
    def __init__(self):
        self.commands = {}

    def register_command(self, name: str, func: Callable, required_args: List[str] = (),
                         optional_args: Optional[List[str]] = None):
        LOGGER.debug(f"Registering command {name} with required_args={required_args} and optional_args={optional_args}")
        self.commands[name] = Command(name, func, required_args, optional_args or [])

    def parse_and_execute(self, command_string, ctx=None):
        LOGGER.debug(f"Parsing command {command_string}")

        parts = command_string.split()
        if not parts:
            raise NoCommandError("No command provided")

        command_name, *args = parts

        command_name = command_name.lower()
        if command_name not in self.commands:
            raise UnknownCommandError(command_name)

        command = self.commands[command_name]

        if len(args) < len(command.required_args):
            raise MissingArgumentsError(command_name, command.required_args)

        args_amount = len(command.required_args) + (len(command.optional_args) if command.optional_args else 0)

        # Execute command with both required and optional args
        return command.func(ctx, *args[: args_amount])


#########################################################

CMD = CommandBuilder()


def cmd_help(ctx):
    return I18N.get(Keys.CMD_HELP, ctx.lang)


def cmd_toggle(ctx):
    status = user.toggle_enabled(ctx.user_id)
    if status:
        return I18N.get(Keys.CMD_TOGGLE_ENABLE, ctx.lang)
    else:
        return I18N.get(Keys.CMD_TOGGLE_DISABLE, ctx.lang)


def cmd_lang(ctx, lang: Optional[str] = None):
    if not lang:
        return I18N.get(Keys.AVAILABLE_LANGS, ctx.lang).format(
            ", ".join(map(lambda l: l.value, Langs))
        )

    # validate lang
    lang = Langs.try_from_str(lang)

    if lang is None:
        return I18N.get(Keys.AVAILABLE_LANGS, ctx.lang).format(
            ", ".join(map(lambda l: l.value, Langs))
        )

    lang = lang.value

    user.set_lang(ctx.user_id, lang)
    return I18N.get(Keys.SET_LANG, ctx.lang).format(lang)


def cmd_echo(_, msg, ):
    return msg


def cmd_6(_):
    return "6"


def cmd_114(_):
    return "514"

def cmd_roll(_):
    return random.randint(1, 6)


def cmd_scream(ctx):
    return I18N.get(Keys.CMD_SCREAM, ctx.lang)


def cmd_ping(_):
    return "pong"

def cmd_about(ctx):
    return I18N.get(Keys.CMD_ABOUT, ctx.lang)


CMD.register_command("help", cmd_help)
CMD.register_command("toggle", cmd_toggle)
CMD.register_command("lang", cmd_lang, [], ["lang"])
CMD.register_command("echo", cmd_echo, ["msg"])
CMD.register_command("6", cmd_6)
CMD.register_command("114", cmd_114)
CMD.register_command("roll", cmd_roll)
CMD.register_command("scream", cmd_scream)
CMD.register_command("ping", cmd_ping)
CMD.register_command("about", cmd_about)