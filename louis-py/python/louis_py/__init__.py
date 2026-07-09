"""Python bindings for the louis braille translator."""

import enum
from typing import NamedTuple

from ._louis_py import (
    __version__,
    Direction,
    Translator,
    TranslationResult,
    LouisError,
    TableParseError,
    TranslationError,
)


class TranslationMode(enum.IntFlag):
    NO_CONTRACTIONS = 1 << 0
    COMPBRL_AT_CURSOR = 1 << 1
    DOTS_IO = 1 << 2
    COMPBRL_LEFT_CURSOR = 1 << 3
    UC_BRL = 1 << 4
    NO_UNDEFINED = 1 << 5
    PARTIAL_TRANS = 1 << 6


class EmphasisSpan(NamedTuple):
    class_: str
    start: int  # inclusive
    end: int  # exclusive


__all__ = [
    "__version__",
    "Direction",
    "Translator",
    "TranslationResult",
    "TranslationMode",
    "EmphasisSpan",
    "LouisError",
    "TableParseError",
    "TranslationError",
]
