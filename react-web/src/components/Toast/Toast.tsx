import "./Toast.scss";

const Toast = (props?: any) => {
    return (
        <div className="overlay">
            <div className="toast-wrapper">
                {props?.title ? (<>
                    <h2>{props?.title}</h2><br /><br /><br />
                </>) : null}
                <span>{props?.message || "Something wrong occurred. Please try again."}</span>
                <br /><br />
            </div>
        </div>
    )
}
export default Toast;