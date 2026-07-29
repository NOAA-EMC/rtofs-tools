#!/usr/bin/env python

#  Gemini was used to assist with developing this code.
# The code has been reviewed, edited, and validated by NWS staff.

import argparse
from datetime import datetime

__all__ = ["valid_date", "str2bool"]


# --- utilities ---


def valid_date(s):
    """Validate and parse a date string in YYYY-MM-DD format.

    This function acts as a custom type validator for argparse.

    Parameters
    ----------
    s : str
        A date string to validate.

    Returns
    -------
    datetime.date
        The parsed date object.

    Raises
    ------
    argparse.ArgumentTypeError
        If the input string does not match the expected YYYY-MM-DD format.
    """
    try:
        return datetime.strptime(s, "%Y-%m-%d").date()
    except ValueError:
        msg = f"Not a valid date: '{s}'. Expected format: YYYY-MM-DD."
        raise argparse.ArgumentTypeError(msg)


def str2bool(v):
    """Convert a string representation of truth to a boolean value.

    Accepts common boolean string representations (e.g., 'yes', 'true', '1',
    'y' for True and 'no', 'false', '0', 'n' for False). This function acts
    as a custom type validator for argparse.

    Parameters
    ----------
    v : str or bool
        The value to convert to a boolean.

    Returns
    -------
    bool
        The converted boolean value.

    Raises
    ------
    argparse.ArgumentTypeError
        If the value cannot be parsed into a boolean.
    """
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected (true/false).")
