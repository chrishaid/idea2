from datarobot_helpers.config import EMAIL_CONFIG
import smtplib

GMAIL_USERNAME = EMAIL_CONFIG['gmail_username']
GMAIL_PASSWORD = EMAIL_CONFIG['gmail_app_password']
DEFAULT_RECIPIENT = EMAIL_CONFIG['default_recipient']

def send_email(subject, body, user=GMAIL_USERNAME, pwd=GMAIL_PASSWORD, recipient=DEFAULT_RECIPIENT):
    gmail_user = user
    gmail_pwd = pwd

    FROM = user
    TO = recipient if type(recipient) is list else [recipient]
    SUBJECT = subject
    TEXT = body

    message = 'From: {0}\nTo: {1}\nSubject: {2}\n\n{3}'.format(FROM, ", ".join(TO), SUBJECT, TEXT)
    try:
        with smtplib.SMTP('smtp.gmail.com', 587) as gmail:
            gmail.ehlo()
            gmail.starttls()
            gmail.login(gmail_user, gmail_pwd)

            gmail.sendmail(FROM, TO, message)

    except Exception as e:
        print(e)