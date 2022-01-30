from stellar_sdk import *
from sys import *

try:
    tx = (
        TransactionBuilder(Account("#{address}", 0))
        .add_text_memo("Logging into Veche")
        .build()
    )
    print(tx.to_xdr())
except Exception as e:
    print(e, file=stderr)
    exit(1)
