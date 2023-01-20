import React, { useEffect, useState, useCallback } from "react";
import { useAppDispatch } from "store/store";
import { getProfileDetails } from "store/slices/auth.slice";

import Modal from "components/Modal/Modal";
import Button from "components/Button/Button";
import Loader from "components/Loader/Loader";

import './ConnectWallet.scss';

const wallets: Array<string> = ['lace', 'nami', 'yoroi']

declare global {
    interface Window {
        cardano:any;
    }
}
let CardanoNS = window.cardano;

const ConnectWallet = () => {
    const dispatch = useAppDispatch();
    const [wallet, setWallet] = useState(null)
    const [walletName, setWalletName] = useState("")
    const [address, setAddress] = useState(null)
    const [isOpen, setIsOpen] = useState(false)
    const [walletLoading, setWalletLoading] = useState(false)

    const openConnectWalletModal = useCallback(() => setIsOpen(true),[])

    const onCloseModal = useCallback(() => setIsOpen(false),[]) 

    const loadWallet = async (walletName: string) => {
        try {
            setWalletLoading(true)
            const enabledWallet = await CardanoNS[walletName].enable();
            setWallet(enabledWallet)
            setWalletName(walletName)
            if (enabledWallet) {
                setAddress(await enabledWallet.getChangeAddress())
            }
        } catch (err) {
            // do nothing
            console.log(err);
        }
    }

    useEffect(() => {
        if (address) {
            (async () => {
                try {
                    const response: any = await dispatch(getProfileDetails({"address": address, "wallet": wallet, "walletName": walletName})).catch(handleError)
                    setWalletLoading(false)
                } catch(error) {
                    setWalletLoading(false)
                    handleError(error)
                    // dispatch(clearCache()) 
                }
            })()
        }
    }, [dispatch, address, wallet, walletName])

    return (
        <>
            <Button
                type="button"
                displayStyle="gradient"
                buttonLabel={"Connect Wallet"}
                onClick={openConnectWalletModal}
            />
            <Modal open={isOpen} title="Connect a wallet" onCloseModal={onCloseModal}>
                <div id="walletsContainer">
                    {wallet ? 
                        null : !CardanoNS ? (<span>No wallet extensions installed yet!</span>) :
                        wallets.map((wallet: string, index: number) => {
                            if (CardanoNS && CardanoNS[wallet]) {
                                return (
                                    <div className="card" key={index} onClick={(_) => loadWallet(wallet)}>
                                        <img src={CardanoNS[wallet].icon} alt={CardanoNS[wallet].name} />
                                        <span>{CardanoNS[wallet].name}</span>
                                    </div>
                                )
                            } else {
                                return null
                            }
                        })
                    }
                    { walletLoading ? <Loader /> : null}
                </div>
            </Modal>
        </>
    )
}

export default ConnectWallet;