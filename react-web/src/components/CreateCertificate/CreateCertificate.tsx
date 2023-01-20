import { useState } from "react";
import { useAppSelector } from "store/store";

import Button from "components/Button/Button";
import Modal from "components/Modal/Modal";


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
 } from '@emurgo/cardano-serialization-lib-asmjs';
import Toast from "components/Toast/Toast";
import { fetchData } from "api/api";

let Buffer = require("buffer/").Buffer;

const CreateCertificate = () => {
    const { uuid } = useAppSelector((state) => state.certification);
    const { address, wallet } = useAppSelector((state) => state.auth);
    const [ certifying, setCertifying ] = useState(false);
    const [ certified, setCertified ] = useState(false);
    const [ transactionId, setTransactionId ] = useState("")
    const [ showError, setShowError ] = useState("");
    const [ openModal, setOpenModal ] = useState(false);
    const [ disableCertify, setDisableCertify ] = useState(false);

    const onCloseModal = () => { setOpenModal(false) }

    const handleError = (errorObj: any) => {
        let errorMsg = ''
        if (typeof errorObj === 'string') {
            errorMsg = errorObj + ' Please try again.'
        } else if (errorObj?.info) {
            errorMsg = errorObj.info + ' Please try again.'
        } else if (errorObj?.response?.message) {
            errorMsg = errorObj?.response.message + ' Please try again.'
        } else if (errorObj?.response?.data) {
            errorMsg = errorObj.response.statusText + ' - ' + errorObj.response.data 
        }
        setShowError(errorMsg.length > 50 ? 'Something wrong occurred. Please try again later.' : errorMsg);
        setTimeout(() => { setShowError("") }, 5000)
        setCertifying(false);
        if (errorObj?.response?.status === 403) {
            setDisableCertify(true)
        }
    }

    const triggerSubmitCertificate = async (txnId: string) => {
        const response: any = await fetchData.post('/run/' + uuid + '/certificate' + '?transactionid=' + txnId).catch(handleError)
        try {
            console.log('broadcasted tnx data ', response.data);
            setTransactionId(response.data.transactionId)
            setOpenModal(true)
            setCertifying(false)
            setCertified(true)
        } catch(e) { }
    }

    const triggerGetCertificate = async () => {
        setCertifying(true);
        setShowError("")
        try {
            // const walletAddressRes: any = await fetchData.get('/wallet-address').catch(handleError)
            // const applicationWallet_receiveAddr = walletAddressRes.data;
            /** For mock */
            const applicationWallet_receiveAddr = 'addr_test1qz2rzeqq8n82gajfp35enq3mxhaynx6zhuql2c7yaljr25mfaznfszxu8275k6v7n05w5azzmxahfzdq564xuuyg73pqnqtrrc'
            // To be replaced with API
            const cert_fee_ada = 3
            const cert_fee_lovelace = BigNum.from_str((cert_fee_ada * 1000000).toString())

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

            wallet.getUtxos().then((utxos: any) =>{
                let txnUnspentOutputs = TransactionUnspentOutputs.new()
                utxos.forEach((utxo: any) => {
                    txnUnspentOutputs.add(TransactionUnspentOutput.from_hex(utxo))
                })
                txBuilder.add_output(TransactionOutput.new(Address.from_bech32(applicationWallet_receiveAddr), Value.new(cert_fee_lovelace) ))
                txBuilder.add_inputs_from(txnUnspentOutputs, CoinSelectionStrategyCIP2.LargestFirst)
                txBuilder.add_change_if_needed(Address.from_hex(address))

                const encodedTx = Buffer.from(txBuilder.build_tx().to_bytes()).toString("hex");
                wallet.signTx(encodedTx).then((signed: string) =>{
                    const txVkeyWitnesses = TransactionWitnessSet.from_bytes(
                        Buffer.from(signed, "hex")
                    );
                    const txSigned = Transaction.new(txBuilder.build(), txVkeyWitnesses );
                    const encodedSignedTx = Buffer.from(txSigned.to_bytes()).toString("hex");
                    wallet.submitTx(encodedSignedTx).then((txnId: string) => {
                        console.log(' transaction id - ', txnId)
                        triggerSubmitCertificate(txnId)
                    }).catch(handleError)
                }).catch(handleError)
            }).catch(handleError)
        } catch (e) {
            handleError(e)
        }
    }

    return (<>
        {certified || disableCertify ? null : (<Button
            displayStyle="gradient"
            onClick={() => triggerGetCertificate()}
            buttonLabel="Get Certificate"
            showLoader={certifying}
        />)}
        {transactionId ? (
            <Modal open={openModal} title="Certification Successful" onCloseModal={onCloseModal}>
                <span>
                    View your certification broadcasted on-chain&nbsp;
                    <a target="_blank" rel="noreferrer" href={`https://preprod.cardanoscan.io/transaction/${transactionId}`}>here</a>!
                </span>
            </Modal>
        ): null}
        {showError ? <Toast message={showError} /> : null}
    </>);
}

export default CreateCertificate;