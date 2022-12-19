import "./Toast.scss";

const Toast = (props?: any) => {
    return (
        <div className="overlay">
            <div className="toast-wrapper">
                <span>{props?.message || "Something wrong occurred. Please try again."}</span>
            </div>
        </div>
    )
}
export default Toast;