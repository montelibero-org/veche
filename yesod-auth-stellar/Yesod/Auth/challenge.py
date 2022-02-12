from stellar_sdk import *
from sys import *

try:
    tx = (
        TransactionBuilder(Account("#{address}", 0))
        .add_text_memo("Logging into Veche")
        .append_manage_data_op('nonce', '#{nonce}')
        .build()
    )
    print(tx.to_xdr())
except Exception as e:
    print(e, file=stderr)
    exit(1)
