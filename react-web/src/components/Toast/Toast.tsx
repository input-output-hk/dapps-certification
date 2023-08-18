import "./Toast.scss";

const Toast = (props?: any) => {
    return (
        <div className="overlay" data-testid="toast">
            <div className="toast-wrapper">
                {props?.title ? (<>
                    <h2 data-testid="toast-title">{props?.title}</h2><br /><br /><br />
                </>) : null}
                <span data-testid="toast-message">{props?.message || "Something wrong occurred. Please try again."}</span>
                <br /><br />
            </div>
        </div>
    )
}
export default Toast;