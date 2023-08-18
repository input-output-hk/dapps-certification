import { act, fireEvent, screen } from "@testing-library/react";

import { renderWithStoreAndRouter } from "test/customTestRenderHook";
import Certification from "./Certification";
import * as customStoreHooks from "store/store";
// import * as fetchData from "api/api";
import { fetchData, postData } from "api/api";

const mockUserDetails = {
  address:
    "addr_test1qqdv6lp2etgw2f8pkhm55unzc3mgn2cs7swmm54zvnlwczxdnjf06y7p78krg2c3nhyl6e0mdttplsugnntmqdza7tnq80ds5n",
  authors: "cgjgcvj",
  contacts: "vbjv",
  dapp: {
    name: "Test",
    owner: "Ali-Hill",
    repo: "plutus-apps",
    version: "1.0",
  },
};
const mockUuid = "cfa480c4-6345-4aa8-8a99-b9adb5f2d46f";
const mockSubscribedFeature = ["l1-run", "l2-upload-report"] as any;

const useLogPreparingMock = {
  startTime: "2023-06-26T03:19:42.805761232Z",
  endTime: "2023-06-26T03:19:42.807191861Z",
  runState: "preparing",
  ended: 0,
};

jest.mock("assets/images/download.svg", () => <span>Mocked svg</span>);

jest.mock("api/api");

jest.useFakeTimers();

describe("Certification component", () => {
  it("renders fields properly", () => {
    const useAppSelectorMock = jest.spyOn(customStoreHooks, "useAppSelector");
    useAppSelectorMock.mockImplementation((selectorFn) =>
      selectorFn({
        certification: {
          uuid: "",
        },
        auth: {
          isLoggedIn: true,
          address: "mock-address",
          wallet: null,
          userDetails: mockUserDetails,
          loading: false,
          network: null,
          subscribedFeatures: mockSubscribedFeature,
        },
        deleteTestHistory: undefined as any,
        runTime: {
          ...useLogPreparingMock,
          buildInfo: {
            runTime: "",
            runState: "",
          },
        },
        walletTransaction: {
          loading: false,
          error: undefined,
        },
        repoAccess: undefined as any,
      })
    );

    renderWithStoreAndRouter(<Certification />);
    expect(screen.queryByTestId("commit-wrapper")).toBeInTheDocument();
    expect(screen.queryByTestId("commit-container")).toBeInTheDocument();
    expect(screen.queryByTestId("commit")).toBeInTheDocument();
  });

  // Note: Test case fails due to state update failure
  it("renders empty banner if user does not have `l1-run` privilege", () => {
    const useAppSelectorMock = jest.spyOn(customStoreHooks, "useAppSelector");
    useAppSelectorMock.mockImplementation((selectorFn) =>
      selectorFn({
        certification: {
          uuid: "",
        },
        auth: {
          isLoggedIn: true,
          address: "mock-address",
          wallet: null,
          userDetails: mockUserDetails,
          loading: false,
          network: null,
          subscribedFeatures: [],
        },
        deleteTestHistory: undefined as any,
        runTime: {
          ...useLogPreparingMock,
          buildInfo: {
            runTime: "",
            runState: "",
          },
        },
        walletTransaction: {
          loading: false,
          error: undefined,
        },
        repoAccess: undefined as any,
      })
    );

    renderWithStoreAndRouter(<Certification />);

    expect(
      screen.queryByText(
        /You do not have a valid subscription to perform the test run/i
      )
    ).toBeInTheDocument();
  });

  it.skip("successfully starts preparing certification on form submit", async () => {
    const useAppSelectorMock = jest.spyOn(customStoreHooks, "useAppSelector");
    useAppSelectorMock.mockImplementation((selectorFn) =>
      selectorFn({
        certification: {
          uuid: mockUuid,
        },
        auth: {
          isLoggedIn: true,
          address: "mock-address",
          wallet: null,
          userDetails: mockUserDetails,
          loading: false,
          network: null,
          subscribedFeatures: mockSubscribedFeature,
        },
        deleteTestHistory: undefined as any,
        runTime: {
          ...useLogPreparingMock,
          buildInfo: {
            runTime: "",
            runState: "",
          },
        },
        walletTransaction: {
          loading: false,
          error: undefined,
        },
        repoAccess: undefined as any,
      })
    );

    // @ts-ignore
    fetchData.get.mockResolvedValue({
      data: [
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-26T03:19:42.397483549Z",
          finishedAt: "2023-06-26T03:21:31.577657102Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "bf640132-7d49-4a4b-b26b-53e5b84f95ed",
          runStatus: "succeeded",
          syncedAt: "2023-06-26T03:21:31.577657102Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-26T03:14:18.878743178Z",
          finishedAt: "2023-06-26T03:17:53.960567052Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "8a243378-711a-40cb-b949-94bbcb74eb3c",
          runStatus: "succeeded",
          syncedAt: "2023-06-26T03:17:53.960567052Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-26T02:38:03.928647151Z",
          finishedAt: "2023-06-26T02:39:28.552331666Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "67d4632b-933d-44c2-b2cc-910daa6d353e",
          runStatus: "succeeded",
          syncedAt: "2023-06-26T02:39:28.552331666Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-26T02:30:34.120369809Z",
          finishedAt: "2023-06-26T02:32:12.458911294Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "cc21245a-b28f-4c0a-a2e5-0cb1e5a1b54e",
          runStatus: "succeeded",
          syncedAt: "2023-06-26T02:32:12.458911294Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-25T21:24:14.558152267Z",
          finishedAt: "2023-06-25T21:25:39.801933894Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "a0d2eb93-b339-4ab4-bce0-f0ec6153dba6",
          runStatus: "succeeded",
          syncedAt: "2023-06-25T21:25:39.801933894Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-25T20:57:23.049339341Z",
          finishedAt: "2023-06-25T20:58:48.255246526Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "d878af77-d530-4b7a-8579-19411a3367cd",
          runStatus: "succeeded",
          syncedAt: "2023-06-25T20:58:48.255246526Z",
        },
        {
          certificationPrice: 1000000,
          commitDate: "2023-02-20T16:59:27Z",
          commitHash: "a8de6d01bf95bb918006063477cc080583870463",
          created: "2023-06-25T19:47:37.434422461Z",
          finishedAt: "2023-06-25T19:49:01.853890323Z",
          repoUrl: "github:Ali-Hill/plutus-apps/a8de6d01bf95",
          reportContentId: null,
          runId: "c6ecb37d-3e29-410d-af46-3a0c85f5235b",
          runStatus: "succeeded",
          syncedAt: "2023-06-25T19:49:01.853890323Z",
        },
      ],
    });

    // @ts-ignore
    postData.post.mockResolvedValue({
      data: "67d00e2c-fb6e-4d90-96c5-61cf7dc3f7a1",
    });

    window.HTMLElement.prototype.scrollIntoView = jest.fn(); // scrollIntoView is not implemented in jsdom

    renderWithStoreAndRouter(<Certification />);

    const inputField = screen.getByTestId("commit");
    const submitBtn = screen.getByTestId("submit");

    expect(submitBtn).toBeDisabled();
    expect(screen.queryByTestId("abort")).not.toBeInTheDocument();

    await act(() => {
      fireEvent.change(inputField, {
        target: { value: "a8de6d01bf95" },
      });
    });

    jest.advanceTimersByTime(5000);

    expect(inputField).toHaveValue("a8de6d01bf95"); // Assert input field has value
    expect(submitBtn).toBeEnabled();

    await act(async () => {
      // Simulate form submission
      fireEvent.click(submitBtn);
    });

    jest.advanceTimersByTime(5000);

    // screen.debug();

    // When form is submitting
    expect(screen.queryByTestId("resultContainer")).toBeInTheDocument();
    expect(screen.queryByTestId("abort")).toBeInTheDocument();
  });
});