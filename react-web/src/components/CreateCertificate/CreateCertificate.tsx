import { useEffect, useState } from "react";
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
 } from '@emurgo/cardano-serialization-lib-browser';
import Toast from "components/Toast/Toast";
import { fetchData } from "api/api";

export interface Run {
    "certificationPrice": number,
    "commitDate": string;
    "commitHash": string;
    "created": string;
    "finishedAt": string;
    "repoUrl": string;
    "reportContentId": string;
    "runId": string;
    "runStatus": "queued" | "failed" | "succeeded" | "certified" | "ready-for-certification" | "aborted";
    "syncedAt": string;
}

interface Certificate {
    "createdAt": string;
    "runId": string;
    "transactionId": string;
}

const CreateCertificate = () => {
    const { uuid } = useAppSelector((state) => state.certification);
    const { address, wallet } = useAppSelector((state) => state.auth);
    const [ certifying, setCertifying ] = useState(false);
    const [ certified, setCertified ] = useState(false);
    const [ transactionId, setTransactionId ] = useState("")
    const [ showError, setShowError ] = useState("");
    const [ openModal, setOpenModal ] = useState(false);
    const [ disableCertify, setDisableCertify ] = useState(false);
    const [certificationPrice, setCertificationPrice] = useState(0);

    // to run only once initially
    useEffect(() => {
        fetchData.get('/profile/current/balance').then(response => {
            const availableProfileBalance: number = response.data
            fetchData.get('/run/' + uuid + '/details').then(res => {
                const runDetails: Run = res.data
                if ((availableProfileBalance - runDetails.certificationPrice) < 0) {
                    setCertificationPrice(runDetails.certificationPrice);
                }
            })
        })
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])
    
    const onCloseModal = () => { setOpenModal(false) }

    const convertAdaToLovelace = (fee_ada: number) => {
        return BigNum.from_str((fee_ada * 1000000).toString())
    }

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
        const timeout = setTimeout(() => { clearTimeout(timeout); setShowError("") }, 5000)
        setCertifying(false);
        if (errorObj?.response?.status === 403) {
            setDisableCertify(true)
        }
    }

    const certificationBroadcasted = (data: Certificate) => {
        console.log('broadcasted tnx data ', data);
        setTransactionId(data.transactionId)
        setOpenModal(true)
        setCertifying(false)
        setCertified(true)
    }

    const fetchRunDetails = async (txnId?: string) => {
        fetchData.get('/run/' + uuid + '/details').then(response => {
            const details: Run = response.data
            if (details?.runStatus === 'ready-for-certification') {
                const timeout = setTimeout(async ()=> {
                    clearTimeout(timeout)
                    fetchRunDetails()
                }, 1000)
            } else if (details?.runStatus === 'certified') {
                fetchData.get('/run/' + uuid + '/certificate' + (txnId ? '?transactionid=' + txnId : ''))
                    .catch(handleError)
                    .then((response: any) => {
                        certificationBroadcasted(response.data)
                    })
            }
        })
    }

    const triggerSubmitCertificate = async (txnId?: string) => {
        fetchData.post('/run/' + uuid + '/certificate' + (txnId ? '?transactionid=' + txnId : ''))
            .catch(handleError)
            .then((response: any) => {
                fetchRunDetails(txnId)
            })
    }

    const triggerGetCertificate = async () => {
        setCertifying(true);
        setShowError("")
        if (certificationPrice) {
            triggerTransactionFromWallet(certificationPrice)
        } else {
            triggerSubmitCertificate()
        }
    }
    
    const triggerTransactionFromWallet = async (cert_fee_in_lovelaces: number) => {
        try {
            const walletAddressRes: any = await fetchData.get('/wallet-address').catch(handleError)
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

            wallet.getUtxos().then((utxos: any) =>{
                let txnUnspentOutputs = TransactionUnspentOutputs.new()
                utxos.forEach((utxo: any) => {
                    txnUnspentOutputs.add(TransactionUnspentOutput.from_hex(utxo))
                })
                txBuilder.add_output(TransactionOutput.new(Address.from_bech32(applicationWallet_receiveAddr), Value.new(cert_fee_lovelace) ))
                txBuilder.add_inputs_from(txnUnspentOutputs, CoinSelectionStrategyCIP2.LargestFirst)
                txBuilder.add_change_if_needed(Address.from_bech32(address))

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
            buttonLabel={"Purchase a Certificate" + (certificationPrice ? " " + (certificationPrice/1000000).toString() + " ADA" : "")}
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