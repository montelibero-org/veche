from stellar_sdk import *
from sys import *

envelope = (
    TransactionEnvelope.from_xdr('#{transactionXdr}', '#{networkPassphrase}')
)
envelope.sign('#{secretKey}')
print(envelope.to_xdr())
