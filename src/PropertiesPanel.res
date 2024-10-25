%raw(`require("./PropertiesPanel.css")`)

module Collapsible = {
  @react.component
  let make = (~title, ~children) => {
    let (collapsed, toggle) = React.useState(() => false)

    <section className="Collapsible">
      <button className="Collapsible-button" onClick={_e => toggle(_ => !collapsed)}>
        <span> {React.string(title)} </span> <span> {React.string(collapsed ? "+" : "-")} </span>
      </button>
      {collapsed ? React.null : <div className="Collapsible-content"> {children} </div>}
    </section>
  }
}

// This component provides a simplified example of fetching JSON data from
// the backend and rendering it on the screen.
module ViewExamples = {
  // Type of the data returned by the /examples endpoint
  type example = {
    id: int,
    some_int: int,
    some_text: string,
  }

  @react.component
  let make = () => {
    let (examples: option<array<example>>, setExamples) = React.useState(_ => None)

    React.useEffect1(() => {
      // Fetch the data from /examples and set the state when the promise resolves
      Fetch.fetchJson(`http://localhost:12346/examples`)
      |> Js.Promise.then_(examplesJson => {
        // NOTE: this uses an unsafe type cast, as safely parsing JSON in rescript is somewhat advanced.
        Js.Promise.resolve(setExamples(_ => Some(Obj.magic(examplesJson))))
      })
      // The "ignore" function is necessary because each statement is expected to return `unit` type, but Js.Promise.then return a Promise type.
      |> ignore
      None
    }, [setExamples])

    <div>
      {switch examples {
      | None => React.string("Loading examples....")
      | Some(examples) =>
        examples
        ->Js.Array2.map(example =>
          React.string(`Int: ${example.some_int->Js.Int.toString}, Str: ${example.some_text}`)
        )
        ->React.array
      }}
    </div>
  }
}

module ControlPaddingAndMargin = {
  type inputState = Default | Changed | Focused
  type metric = Px | Pt | Percent

  type formDataState = {
    marginTopS: inputState,
    marginRightS: inputState,
    marginBottomS: inputState,
    marginLeftS: inputState,
    paddingTopS: inputState,
    paddingRightS: inputState,
    paddingBottomS: inputState,
    paddingLeftS: inputState,
  }

  type formData = {
    marginTop: string,
    marginRight: string,
    marginBottom: string,
    marginLeft: string,
    paddingTop: string,
    paddingRight: string,
    paddingBottom: string,
    paddingLeft: string,
  }

  let styles = {
    "boxContainer": ReactDOM.Style.make(
      ~display="flex",
      ~justifyContent="center",
      ~alignItems="center",
      ~width="300px",
      ~height="300px",
      (),
    ),
    "box": ReactDOM.Style.make(
      ~position="relative",
      ~width="200px",
      ~height="200px",
      ~border="5px solid #28334D",
      (),
    ),
    "input": ReactDOM.Style.make(~width="60px", ~position="absolute", ~fontSize="12px", ()),
    "inputDefault": ReactDOM.Style.make(~border="1px solid #ccc", ()),
    "inputChanged": ReactDOM.Style.make(~border="1px solid yellow", ()),
    "inputFocused": ReactDOM.Style.make(~border="2px solid blue", ()),
    "marginTop": ReactDOM.Style.make(~top="-30px", ~left="50%", ~transform="translateX(-50%)", ()),
    "marginBottom": ReactDOM.Style.make(
      ~bottom="-30px",
      ~left="50%",
      ~transform="translateX(-50%)",
      (),
    ),
    "marginLeft": ReactDOM.Style.make(~left="-65px", ~top="50%", ~transform="translateY(-50%)", ()),
    "marginRight": ReactDOM.Style.make(
      ~right="-65px",
      ~top="50%",
      ~transform="translateY(-50%)",
      (),
    ),
    "paddingTop": ReactDOM.Style.make(~top="5px", ~left="50%", ~transform="translateX(-50%)", ()),
    "paddingBottom": ReactDOM.Style.make(
      ~bottom="5px",
      ~left="50%",
      ~transform="translateX(-50%)",
      (),
    ),
    "paddingLeft": ReactDOM.Style.make(~left="5px", ~top="50%", ~transform="translateY(-50%)", ()),
    "paddingRight": ReactDOM.Style.make(
      ~right="5px",
      ~top="50%",
      ~transform="translateY(-50%)",
      (),
    ),
    "content": ReactDOM.Style.make(
      ~position="absolute",
      ~top="50%",
      ~left="50%",
      ~transform="translate(-50%, -50%)",
      ~fontSize="14px",
      (),
    ),
    "valueDisplay": ReactDOM.Style.make(
      ~position="absolute",
      ~fontSize="12px",
      ~cursor="pointer",
      (),
    ),
  }

  let getInputStyle = state => {
    switch state {
    | Default => ReactDOM.Style.combine(styles["input"], styles["inputDefault"])
    | Changed => ReactDOM.Style.combine(styles["input"], styles["inputChanged"])
    | Focused => ReactDOM.Style.combine(styles["input"], styles["inputFocused"])
    }
  }

  let parseValue = value => {
    let numericValue = Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)->int_of_string
    let metric = if Js.String.includes("%", value) {
      Percent
    } else if Js.String.includes("pt", value) {
      Pt
    } else {
      Px
    }
    (numericValue, metric)
  }

  let formatValue = (value, metric) => {
    switch metric {
    | Percent => value->string_of_int ++ "%"
    | Pt => value->string_of_int ++ "pt"
    | Px => value->string_of_int ++ "px"
    }
  }

  @react.component
  let make = () => {
    let initialFormData = {
      marginTop: "",
      marginRight: "",
      marginBottom: "",
      marginLeft: "",
      paddingTop: "",
      paddingRight: "",
      paddingBottom: "",
      paddingLeft: "",
    }

    let initialFormDataState = {
      marginTopS: Default,
      marginRightS: Default,
      marginBottomS: Default,
      marginLeftS: Default,
      paddingTopS: Default,
      paddingRightS: Default,
      paddingBottomS: Default,
      paddingLeftS: Default,
    }

    let (formData, setFormData) = React.useState(_ => initialFormData)
    let (formDataState, setFormDataState) = React.useState(_ => initialFormDataState)

    let handleInputChange = (key, value) => {
      let (numericValue, metric) = parseValue(value)
      let formattedValue = formatValue(numericValue, metric)
      setFormData(prev => {
        switch key {
        | "marginTop" => {...prev, marginTop: formattedValue}
        | "marginRight" => {...prev, marginRight: formattedValue}
        | "marginBottom" => {...prev, marginBottom: formattedValue}
        | "marginLeft" => {...prev, marginLeft: formattedValue}
        | "paddingTop" => {...prev, paddingTop: formattedValue}
        | "paddingRight" => {...prev, paddingRight: formattedValue}
        | "paddingBottom" => {...prev, paddingBottom: formattedValue}
        | "paddingLeft" => {...prev, paddingLeft: formattedValue}
        | _ => prev
        }
      })
    }

    let handleFocus = key => {
      setFormDataState(prev => {
        switch key {
        | "marginTop" => {...prev, marginTopS: Focused}
        | "marginRight" => {...prev, marginRightS: Focused}
        | "marginBottom" => {...prev, marginBottomS: Focused}
        | "marginLeft" => {...prev, marginLeftS: Focused}
        | "paddingTop" => {...prev, paddingTopS: Focused}
        | "paddingRight" => {...prev, paddingRightS: Focused}
        | "paddingBottom" => {...prev, paddingBottomS: Focused}
        | "paddingLeft" => {...prev, paddingLeftS: Focused}
        | _ => prev
        }
      })
    }

    let handleBlur = key => {
      setFormDataState(prev => {
        switch key {
        | "marginTop" => {...prev, marginTopS: formData.marginTop !== "" ? Changed : Default}
        | "marginRight" => {...prev, marginRightS: formData.marginRight !== "" ? Changed : Default}
        | "marginBottom" => {
            ...prev,
            marginBottomS: formData.marginBottom !== "" ? Changed : Default,
          }
        | "marginLeft" => {...prev, marginLeftS: formData.marginLeft !== "" ? Changed : Default}
        | "paddingTop" => {...prev, paddingTopS: formData.paddingTop !== "" ? Changed : Default}
        | "paddingRight" => {
            ...prev,
            paddingRightS: formData.paddingRight !== "" ? Changed : Default,
          }
        | "paddingBottom" => {
            ...prev,
            paddingBottomS: formData.paddingBottom !== "" ? Changed : Default,
          }
        | "paddingLeft" => {...prev, paddingLeftS: formData.paddingLeft !== "" ? Changed : Default}
        | _ => prev
        }
      })
    }

    let renderInput = (key, style) => {
      let stateKey = switch key {
      | "marginTop" => formDataState.marginTopS
      | "marginRight" => formDataState.marginRightS
      | "marginBottom" => formDataState.marginBottomS
      | "marginLeft" => formDataState.marginLeftS
      | "paddingTop" => formDataState.paddingTopS
      | "paddingRight" => formDataState.paddingRightS
      | "paddingBottom" => formDataState.paddingBottomS
      | "paddingLeft" => formDataState.paddingLeftS
      | _ => Default
      }

      let value = switch key {
      | "marginTop" => formData.marginTop
      | "marginRight" => formData.marginRight
      | "marginBottom" => formData.marginBottom
      | "marginLeft" => formData.marginLeft
      | "paddingTop" => formData.paddingTop
      | "paddingRight" => formData.paddingRight
      | "paddingBottom" => formData.paddingBottom
      | "paddingLeft" => formData.paddingLeft
      | _ => ""
      }

      switch stateKey {
      | Focused =>
        <input
          type_="text"
          style={ReactDOM.Style.combine(getInputStyle(stateKey), style)}
          value={value}
          onChange={e => handleInputChange(key, ReactEvent.Form.target(e)["value"])}
          onBlur={_ => handleBlur(key)}
          autoFocus={true}
        />
      | _ =>
        <div
          style={ReactDOM.Style.combine(styles["valueDisplay"], style)}
          onClick={_ => handleFocus(key)}>
          {React.string(value !== "" ? value : "auto")}
        </div>
      }
    }

    <div style={styles["boxContainer"]}>
      <div style={styles["box"]}>
        {renderInput("marginTop", styles["marginTop"])}
        {renderInput("marginBottom", styles["marginBottom"])}
        {renderInput("marginLeft", styles["marginLeft"])}
        {renderInput("marginRight", styles["marginRight"])}
        {renderInput("paddingTop", styles["paddingTop"])}
        {renderInput("paddingBottom", styles["paddingBottom"])}
        {renderInput("paddingLeft", styles["paddingLeft"])}
        {renderInput("paddingRight", styles["paddingRight"])}
        <div style={styles["content"]}> {React.string("Content")} </div>
      </div>
    </div>
  }
}

@genType @genType.as("PropertiesPanel") @react.component
let make = () =>
  <aside className="PropertiesPanel">
    <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding"> <ControlPaddingAndMargin /> </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
