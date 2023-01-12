// import { Lucid } from "https://deno.land/x/lucid/mod.ts"
import { Lucid } from 'lucid-cardano';

import { useAppSelector } from "store/store";

import Button from "components/Button/Button";
// import {  } from "./CreateCertification.helper";

const CreateCertificate = () => {
    const { uuid } = useAppSelector((state) => state.certification);
    const { address, wallet } = useAppSelector((state) => state.auth);

    const triggerGetCertificate = async () => {
        console.log(uuid, address);

        // To be replaced with API
        const applicationWallet_receiveAddr = 'addr_test1qz2rzeqq8n82gajfp35enq3mxhaynx6zhuql2c7yaljr25mfaznfszxu8275k6v7n05w5azzmxahfzdq564xuuyg73pqnqtrrc';
        // To be replaced with API
        const cert_fee_ada = 3
        const cert_fee_lovelace = BigInt((cert_fee_ada * 1000000).toString())

        
        const lucid: Lucid = await Lucid.new(undefined, "Preview");
          
        lucid.selectWallet(wallet);
        
        const tx = await lucid.newTx()
        .payToAddress(applicationWallet_receiveAddr, { lovelace: cert_fee_lovelace })
        .complete();
        
        const signedTx = await tx.sign().complete();
        
        const txHash = await signedTx.submit();
        // To be sent in POST /run/{uuid}/certificate
        console.log(txHash);

    }

    return (<>
        <Button
            displayStyle="gradient"
            onClick={() => triggerGetCertificate()}
            buttonLabel="Get Certificate"
        />
    </>);
}

export default CreateCertificate;