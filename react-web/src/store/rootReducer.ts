import { combineReducers } from "@reduxjs/toolkit";

import authReducer from "./slices/auth.slice";
import certificationReducer from "pages/certification/slices/certification.slice";
import deleteTestHistory from "pages/testHistory/slices/deleteTestHistory.slice";

const rootReducer = combineReducers({
  auth: authReducer,
  certification: certificationReducer,
  deleteTestHistory
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;