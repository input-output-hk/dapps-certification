import { createSlice, createAsyncThunk } from "@reduxjs/toolkit";
import { fetchData } from "api/api";

import { Address,
    Value,
    BigNum,
    LinearFee,
    TransactionBuilderConfigBuilder,
    TransactionUnspentOutputs,
    TransactionUnspentOutput,
    TransactionBuilder,
    TransactionWitnessSet,
    Transaction,
    TransactionOutput,
    CoinSelectionStrategyCIP2
 } from '@emurgo/cardano-serialization-lib-browser';

const initialState: {loading: boolean; error: any;} = {
  loading: false,
  error: false
};

export const payFromWallet: any = createAsyncThunk("payFromWallet", (data: any, { rejectWithValue }) => {
    return new Promise(async (resolve: any, reject) => {
        const throwError = (errorObj: any) => {
            reject(errorObj)
        }
        try {
            const cert_fee_in_lovelaces = data.fee
            const walletAddressRes: any = await fetchData.get('/wallet-address').catch(throwError)
            const applicationWallet_receiveAddr = walletAddressRes.data;
            const cert_fee_lovelace: BigNum = BigNum.from_str(cert_fee_in_lovelaces.toString())
            /** For mock */
            // const applicationWallet_receiveAddr = 'addr_test1qz2rzeqq8n82gajfp35enq3mxhaynx6zhuql2c7yaljr25mfaznfszxu8275k6v7n05w5azzmxahfzdq564xuuyg73pqnqtrrc'
            // const cert_fee_ada = 3
            // const cert_fee_lovelace = convertAdaToLovelace(cert_fee_ada)

            const protocolParams: any = {
                linearFee: {
                    minFeeA: "440",
                    minFeeB: "175381",
                },
                minUtxo: "34482",
                poolDeposit: "500000000",
                keyDeposit: "2000000",
                maxValSize: 5000,
                maxTxSize: 16384,
                priceMem: 0.0577,
                priceStep: 0.0000721,
                // minFeeCoefficient: 44,
                // minFeeConstant: 155_381,
                coinsPerUtxoByte: "4310"
            } 

            let linearFee = LinearFee.new(
                BigNum.from_str(protocolParams.linearFee.minFeeA),
                BigNum.from_str(protocolParams.linearFee.minFeeB)
            );
            let txnBuilderConfigBuilder = TransactionBuilderConfigBuilder.new()
                .fee_algo(linearFee)
                .coins_per_utxo_byte(BigNum.from_str(protocolParams.coinsPerUtxoByte))
                .key_deposit(BigNum.from_str(protocolParams.keyDeposit))
                .pool_deposit(BigNum.from_str(protocolParams.poolDeposit))
                .max_value_size(protocolParams.maxValSize)
                .max_tx_size(protocolParams.maxTxSize)
            
            let txBuilder = TransactionBuilder.new(txnBuilderConfigBuilder.build())

            data.wallet.getUtxos().then((utxos: any) =>{
                let txnUnspentOutputs = TransactionUnspentOutputs.new()
                utxos.forEach((utxo: any) => {
                    txnUnspentOutputs.add(TransactionUnspentOutput.from_hex(utxo))
                })
                txBuilder.add_output(TransactionOutput.new(Address.from_bech32(applicationWallet_receiveAddr), Value.new(cert_fee_lovelace) ))
                txBuilder.add_inputs_from(txnUnspentOutputs, CoinSelectionStrategyCIP2.LargestFirst)
                txBuilder.add_change_if_needed(Address.from_bech32(data.address))

                const encodedTx = Buffer.from(txBuilder.build_tx().to_bytes()).toString("hex");
                data.wallet.signTx(encodedTx).then((signed: string) =>{
                    const txVkeyWitnesses = TransactionWitnessSet.from_bytes(
                        Buffer.from(signed, "hex")
                    );
                    const txSigned = Transaction.new(txBuilder.build(), txVkeyWitnesses );
                    const encodedSignedTx = Buffer.from(txSigned.to_bytes()).toString("hex");
                    data.wallet.submitTx(encodedSignedTx).then((txnId: string) => {
                        resolve(txnId);
                    }).catch(throwError)
                }).catch(throwError)
            }).catch(throwError)
        } catch (err) { throwError(err) }
    })
})

export const walletTransactionSlice = createSlice({
  name: "walletTransaction",
  initialState,
  reducers: {
    handleError: (state, errorObj?) => {
        state.error = errorObj
    }
  },
  extraReducers: (builder) => {
    builder
        .addCase(payFromWallet.pending, (state) => {
            state.error = false;
            state.loading = true;
        })
        .addCase(payFromWallet.fulfilled, (state, actions) => {
            state.error = false;
        })
        .addCase(payFromWallet.rejected, (state, actions) => {
            handleError(actions.error)
            state.loading = false
        })
  }
});


export const { handleError } = walletTransactionSlice.actions;

export default walletTransactionSlice.reducer;
