import { combineReducers } from "@reduxjs/toolkit";

import authReducer from "./slices/auth.slice";
import certificationReducer from "pages/certification/slices/certification.slice";

const rootReducer = combineReducers({
  auth: authReducer,
  certification: certificationReducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;