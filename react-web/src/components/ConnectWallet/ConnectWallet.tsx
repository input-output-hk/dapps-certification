import React, { useEffect, useState } from "react";
import './ConnectWallet.scss';
import Modal from "components/Modal/Modal";
import Button from "components/Button/Button";

import { useAppDispatch } from "store/store";
import { getProfileDetails } from "store/slices/auth.slice";
import Toast from "components/Toast/Toast";


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
            <Toast message={""}/>
        }
    }

    const bindAccountChange = () => {
        if (CardanoNS?.onAccountChange && typeof CardanoNS.onAccountChange === 'function') { 
            CardanoNS.onAccountChange((address: Array<any>) => {
                setAddress(address[0]);
            })
        }
    }
    bindAccountChange();

    useEffect(() => {
        if (address) {
            dispatch(getProfileDetails({"address": address}))            
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [address])
    

    const [isOpen, setIsOpen] = useState(false)
    const openConnectWalletModal = () => {
        setIsOpen(true)
    }

    const onCloseModal = (flag: boolean) => {
        setIsOpen(flag)
    } 


    return (
        <>
            <Button
                type="button"
                displayStyle="gradient"
                buttonLabel={"Connect Wallet"}
                onClick={(_) => openConnectWalletModal()}
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