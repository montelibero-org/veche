from stellar_sdk import *
from sys import *

MAINNET_PASSPHRASE = 'Public Global Stellar Network ; September 2015'

# print(repr('#{transactionXdr}'), file=stderr)
# print(repr('#{secretKey}'), file=stderr)
envelope = TransactionEnvelope.from_xdr('#{transactionXdr}', MAINNET_PASSPHRASE)
envelope.sign('#{secretKey}')
print(envelope.to_xdr())
