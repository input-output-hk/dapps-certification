import "./Toast.scss";

const Toast = () => {
    return (
        <div className="overlay">
            <div className="toast-wrapper">
                <span>Something wrong occurred. Please try again.</span>
            </div>
        </div>
    )
}
export default Toast;