from stellar_sdk import *
from sys import *

try:
    envelope = (
        TransactionEnvelope
        .from_xdr(
            '#{envelope}',
            'Public Global Stellar Network ; September 2015'
        )
    )
    account_id = envelope.transaction.source.account_id
    keypair = Keypair.from_public_key(account_id)
    [signature] = envelope.signatures
    keypair.verify(envelope.hash(), signature.signature)
    print(account_id)
except Exception as e:
    print(e, file=stderr)
    exit(1)
