import React, { useEffect, useState } from "react";
import './ConnectWallet.scss'

const wallets: Array<string> = ['lace', 'nami', 'yoroi']

declare global {
    interface Window {
        cardano:any;
    }
}
let CardanoNS = window.cardano;

const ConnectWallet = () => {
    const [wallet, setWallet] = useState(null)
    const [address, setAddress] = useState(null)

    const loadWallet = async (walletName: string) => {
        const enabledWallet = await CardanoNS[walletName].enable();
        setWallet(enabledWallet)
        if (enabledWallet) {
            setAddress(await enabledWallet.getChangeAddress())
        }
    }

    useEffect(() => {
        CardanoNS.onAccountChange((address: Array<any>) => {
            setAddress(address[0]);
        })
    });

    return (
        <div className="modal-wrapper">
            <div className="modal-backdrop"></div>
            <div className="modal">
                <div className="modal-body">
                    <div id="walletsContainer">
                        {wallet ? <>
                            <span>address : {address}</span>
                        </> :
                            wallets.map((wallet: string) => {
                                if (CardanoNS[wallet]) {
                                    return (
                                        <div className="card" onClick={(_) => loadWallet(wallet)}>
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
                </div>
            </div>
        </div>
    )
}

export default ConnectWallet;