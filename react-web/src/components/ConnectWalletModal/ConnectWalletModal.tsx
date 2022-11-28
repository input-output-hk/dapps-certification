import React, { useEffect, useState } from "react";
import './ConnectWalletModal.scss';
import CustomizedDialogs from "components/Modal/Modal";

import { useAppDispatch } from "store/store";
import { login } from "store/slices/auth.slice";


const wallets: Array<string> = ['lace', 'nami', 'yoroi']

declare global {
    interface Window {
        cardano:any;
    }
}
let CardanoNS = window.cardano;

const ConnectWalletModal = (props?: any) => {
    const dispatch = useAppDispatch();
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
        if (CardanoNS?.onAccountChange && typeof CardanoNS.onAccountChange === 'function') { 
            CardanoNS.onAccountChange((address: Array<any>) => {
                setAddress(address[0]);
            })
        }
    });

    useEffect(() => {
        address && dispatch(login());
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [address])
    

    // return (
    //     <div className="modal-wrapper">
    //         <div className="modal-backdrop"></div>
    //         <div className="modal">
    //             <div className="modal-body">
    //                 <div id="walletsContainer">
    //                     {wallet ? <>
    //                         <span>address : {address}</span>
    //                     </> :
    //                         wallets.map((wallet: string) => {
    //                             if (CardanoNS[wallet]) {
    //                                 return (
    //                                     <div className="card" onClick={(_) => loadWallet(wallet)}>
    //                                         <img src={CardanoNS[wallet].icon} alt={CardanoNS[wallet].name} />
    //                                         <span>{CardanoNS[wallet].name}</span>
    //                                     </div>
    //                                 )
    //                             } else {
    //                                 return null
    //                             }
    //                         })
    //                     }
    //                 </div>
    //             </div>
    //         </div>
    //     </div>
    // )

    return props?.open ? (<>
            <CustomizedDialogs title="Connect a wallet">
                <div id="walletsContainer">
                    {wallet ? <>
                        <span>address : {address}</span>
                    </> :
                        wallets.map((wallet: string, index: number) => {
                            if (CardanoNS[wallet]) {
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
            </CustomizedDialogs>
        </>) : null
}

export default ConnectWalletModal;