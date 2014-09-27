# This file is a post_process of rss2email
#
# rss2email will encode UTF-8 string with base64 before send mail,
# which is difficult filted by procmail. This post_process will
# add addition headers: X-Pinyin-From header and X-Pinyin-Subject,
# They will store pinyin strings converted From header and Subject header
# before send mail.
#
# Use:
#   1. cp the file to  <your-rss2email-path>/rss2email/post_process/
#      before install rss2email from source.
#   2. install rss2email from source, the source path: <your-rss2email-path>
#   3. add the below line to your rss2email configure:
#
#       post-process = rss2email.post_process.add_pinyin_header add_pinyin_header
#

import rss2email.email
import pinyin

def remove_non_ascii(string):
    """remove no ascii char in string"""
    return "".join([i for i in string if ord(i)<128])

def add_pinyin_header(message, **kwargs):
    header_subject = rss2email.email._decode_header(message['Subject'])
    header_from = rss2email.email._decode_header(message['From'])
    message['X-Pinyin-Subject'] = remove_non_ascii(pinyin.get(header_subject))
    message['X-Pinyin-From'] = remove_non_ascii(pinyin.get(header_from))
    return message
