# https://unix.stackexchange.com/a/48355
import subprocess
def password(acct):
    try:
        return subprocess.check_output(["pass", acct]).strip()
    except subprocess.CalledProcessError:
        return ""
