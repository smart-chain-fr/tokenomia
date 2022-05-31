# Token Distribution

Distribute tokens to a list of addresses.
  
**USAGE**

```
Usage: token-distribution [(-m|--mainnet) | (-t|--testnet MAGIC)]
                          (-i|--distribution-file FILENAME) 
                          [-n|--recipient-per-tx SIZE] (-T|--token-wallet NAME)
                          (-A|--ada-wallet NAME) (-C|--collateral-wallet NAME) 
                          [-e|--min-lovelaces-per-utxo AMOUNT] [-y|--dry-run] 
                          [-v|--verbose]

Available options:
  -m,--mainnet             Use the mainnet network
  -t,--testnet MAGIC       Use this testnet magic id (default: 1097911063)
  -i,--distribution-file FILENAME
                           Recipient addresses and amounts to distribute
  -n,--recipient-per-tx SIZE
                           Batch together multiple addresses (default: 10)
  -T,--token-wallet NAME   Wallet name for token source
  -A,--ada-wallet NAME     Wallet name for Ada source
  -C,--collateral-wallet NAME
                           Wallet name for collateral
  -e,--min-lovelaces-per-utxo AMOUNT
                           Minimum lovelace amount per UTxO (default: 1379280)
  -d,--metadata-file FILENAME
                           Metadata shared by all transactions
  -y,--dry-run             Build transactions without submitting them
  -v,--verbose             Show more details about build processes
  -h,--help                Show this help text
```
  
**INPUT FILE**
  
Provide a `JSON` file containing the list of address and associated amount of token in the following format.
  
```json
{
  "assetClass": {
    "currencySymbol": "2e25113c74ba9916a2a62c83e35e4511dd3a2fd53da1ffbed2ff66ab",
    "tokenName": "CustomToken"
  },
  "recipients": [
    {
      "address": "addr_test1qpjnxcqtpr4x50evj2av729ju5hl4xy8qk80grnx2t0tgngktlq886thdlasr93qxhtuvkqtjahrhgrpsdxxh3h2r2js9aruk4",
      "amount": 100
    },
    {
      "address": "addr_test1qqulgrwz6wxjtyuq2054el8pw8tpaznl63t3gee5gp9qyacktlq886thdlasr93qxhtuvkqtjahrhgrpsdxxh3h2r2jsrj52es",
      "amount": 100
    },
    {
      "address": "addr_test1qr06f4jwm683l9q6qas7pat2vj24vv8r802zn2r7dlexxnqktlq886thdlasr93qxhtuvkqtjahrhgrpsdxxh3h2r2jseca6jx",
      "amount": 100
    }
  ]
}
```

**METADATA FILE**

Provide a `JSON` file with an integer top level key (as per cardano [transaction metadata requirement](https://developers.cardano.org/docs/transaction-metadata/). This random integer can be used as an identifier for the distribution to be retrieved with a metadata cache.

```json
{
  "4204510689596672392": "Cardashift airdrop"
}
```

**PREREQUISITES**

- `recipients` input list must be non empty
- `recipientPerTx` option must be an integer in the range `[0, 70]`
- `amount` must all be strictly positive `> 0`
- `address` must all be unique, no duplicate
- `tokenSource` must already be provisionned with sufficient amount of token
- `adaSource` must already be provisionned with sufficient amount of ADA for fees and minADAperUTxO value
- `collateralSource` must already be provisionned with an UTxO containing 2 ADA

**BENCHMARK**

Date : May 31 2022 </br>
cardano-cli 1.34.1

| Batchs per execution | Tx per batch | Total Tx | Status |
|----------------------|--------------|----------|--------|
| 1                    | 148          | 148      | OK     |
| 40                   | 10           | 400      | OK     |
| 40                   | 70           | 2800     | NOK    |
| 35                   | 70           | 2450     | OK     |
| 35                   | 100          | 3500     | OK     |
