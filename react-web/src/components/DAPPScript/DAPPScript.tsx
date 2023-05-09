import Button from "components/Button/Button"
import { Input } from "compositions/Form/components/Input"
import './DAPPScript.scss'

const DAPPScript = () => {


    return (<>
        <div className="bordered script-item">
            <div className="action-button">
                <Button 
                    displayStyle="primary-outline"
                    size="small"
                    disabled={true}
                    buttonLabel="+ Add Script"
                    onClick={() => {}}/>
            </div>
            <Input
                label="Script Hash"
                type="text"
                id="scriptHash"
                name="scriptHash"
                required={true} />
            <Input
                label="Contact Address"
                type="text"
                id="contactAddress"
                name="contactAddress"
                required={true} />
        </div>
    </>)
}

export default DAPPScript