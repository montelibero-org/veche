from stellar_sdk import *
from sys import *


XDR = '#{envelope}'


MAINNET_PASSPHRASE = 'Public Global Stellar Network ; September 2015'
TESTNET_PASSPHRASE = 'Test SDF Network ; September 2015'


def verify(xdr, passphrase):
    try:
        envelope = TransactionEnvelope.from_xdr(xdr, passphrase)
        account_id = envelope.transaction.source.account_id
        keypair = Keypair.from_public_key(account_id)
        [signature] = envelope.signatures
        keypair.verify(envelope.hash(), signature.signature)
        [nonce] = [
            op.data_value.decode()
            for op in envelope.transaction.operations
            if isinstance(op, ManageData) and op.data_name == 'nonce'
        ]
        return (True, '{}\t{}'.format(account_id, nonce))
    except Exception as e:
        return (False, str(e))


ok, result = verify(XDR, MAINNET_PASSPHRASE)
if ok:
    print(result)
else:
    ok, result = verify(XDR, TESTNET_PASSPHRASE)
    if ok:
        print(result)
    else:
        print(result, file=stderr)
        exit(1)
