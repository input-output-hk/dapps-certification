import React, { ComponentProps, forwardRef, useEffect, useState } from "react";

import { FieldError } from "./FieldError";
import "./Input.scss";
import { useFormContext } from "react-hook-form";

interface InputProps extends ComponentProps<"input"> {
 label: string;
 disabled?: boolean;
 disablefocus?: boolean;
 name: string;
 required?: boolean;
}

export const Input = forwardRef<HTMLInputElement, InputProps>(function Input(
 {
   disabled = false,
   label,
   type = "text",
   className = "",
   disablefocus = false,
   name,
   required = false,
   ...props
 },
 ref
) {
 const {
   formState: { errors },
 } = useFormContext();

 useEffect(() => {
   disablefocus && setActive(true);
 }, [disablefocus]);

 const [active, setActive] = useState(false);

 return (
   <div
     className={`input-wrapper ${className}`}
     onBlur={(e: any) => !e.target.value && setActive(false)}
     onClick={(e: any) => setActive(true)}
     data-testid={`${name}-wrapper`}
   >
     <div
       className={`input ${active ? "active" : ""} ${errors[name] ? "error" : ""} ${disabled ? "disabled" : ""}`}
       onClick={(_) => {
         setActive(true);
         document.getElementById(name || "")?.focus();
       }}
       data-testid={`${name}-container`}
     >
       <label>{label} {required ? <span style={{color: 'red'}}>*</span> : null}</label>
       <input
         type={type}
         ref={ref}
         {...props}
         name={name}
         id={name}
         data-testid={name}
       />
     </div>

     {errors[name]?.message ? <FieldError message={errors[name]?.message} /> : <></>}
   </div>
 );
});
