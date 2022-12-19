import React, { useEffect, useState, useCallback } from "react";
import './ConnectWallet.scss';
import Modal from "components/Modal/Modal";
import Button from "components/Button/Button";

import { useAppDispatch } from "store/store";
import { getProfileDetails, logout } from "store/slices/auth.slice";

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
    const [address, setAddress] = useState(null)
    const [isOpen, setIsOpen] = useState(false)

    const openConnectWalletModal = useCallback(() => setIsOpen(true),[])

    const onCloseModal = useCallback(() => setIsOpen(false),[]) 

    const loadWallet = async (walletName: string) => {
        try {
            const enabledWallet = await CardanoNS[walletName].enable();
            setWallet(enabledWallet)
            if (enabledWallet) {
                setAddress(await enabledWallet.getChangeAddress())
            }
        } catch (err) {
            // do nothing
            console.log(err);
        }
    }

    useEffect(() => {
        if (wallet) {
            const enabledWallet: any = wallet;
            if (address !== enabledWallet.getChangeAddress()) {
                // account has been changed. Force logout the user
                dispatch(logout());
            } else {
                // do nothing
            }
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    useEffect(() => {
        if (address) {
            dispatch(getProfileDetails({"address": address}))            
        }
    }, [dispatch, address])

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
                </div>
            </Modal>
        </>
    )
}

export default ConnectWallet;