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

  type formMetrics = {
    marginTopM: metric,
    marginRightM: metric,
    marginBottomM: metric,
    marginLeftM: metric,
    paddingTopM: metric,
    paddingRightM: metric,
    paddingBottomM: metric,
    paddingLeftM: metric,
  }

  let styles = {
    "boxContainer": ReactDOM.Style.make(
      ~display="flex",
      ~justifyContent="center",
      ~alignItems="center",
      ~width="200px",
      ~height="200px",
      ~paddingLeft="50px",
      (),
    ),
    "box": ReactDOM.Style.make(
      ~position="relative",
      ~width="180px",
      ~height="100px",
      ~border="5px solid #28334D",
      (),
    ),
    "input": ReactDOM.Style.make(
      ~width="30px",
      ~height="18px",
      ~fontSize="12px",
      ~backgroundColor="#28334D",
      ~color="white",
      ~border="none",
      ~outline="none",
      (),
    ),
    "select": ReactDOM.Style.make(
      ~fontSize="12px",
      ~backgroundColor="#28334D",
      ~color="white",
      ~border="none",
      ~outline="none",
      ~marginLeft="1px",
      (),
    ),
    "inputContainer": ReactDOM.Style.make(
      ~display="flex",
      ~alignItems="center",
      ~position="absolute",
      (),
    ),
    "inputDropdownContainer": ReactDOM.Style.make(~position="absolute", ()),
    "marginTop": ReactDOM.Style.make(~top="-20px", ~left="50%", ~transform="translateX(-50%)", ()),
    "marginBottom": ReactDOM.Style.make(
      ~bottom="-20px",
      ~left="50%",
      ~transform="translateX(-50%)",
      (),
    ),
    "marginLeft": ReactDOM.Style.make(~left="-35px", ~top="50%", ~transform="translateY(-50%)", ()),
    "marginRight": ReactDOM.Style.make(
      ~right="-35px",
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
    "valueDisplayChanged": ReactDOM.Style.make(
      ~position="absolute",
      ~borderBottom="1px dotted yellow",
      ~display="flex",
      ~fontSize="12px",
      ~cursor="pointer",
      ~color="white",
      (),
    ),
    "valueDisplayDefault": ReactDOM.Style.make(
      ~position="absolute",
      ~display="flex",
      ~fontSize="12px",
      ~cursor="pointer",
      ~color="white",
      (),
    ),
    "valueDisplayFocused": ReactDOM.Style.make(
      ~position="absolute",
      ~display="flex",
      ~fontSize="12px",
      ~cursor="pointer",
      ~color="white",
      (),
    ),
  }

  let getInputStyle = state => {
    switch state {
    | Default => ReactDOM.Style.combine(styles["input"], ReactDOM.Style.make(~color="white", ()))
    | Changed => ReactDOM.Style.combine(styles["input"], ReactDOM.Style.make(~color="white", ()))
    | Focused =>
      ReactDOM.Style.combine(
        styles["input"],
        ReactDOM.Style.make(
          ~color="white",
          ~borderRadius="2px",
          ~boxShadow="0 0 0 1px #139BD1",
          (),
        ),
      )
    }
  }

  let parseValue = value => {
    let numericValue = Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)
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
    switch (value, metric) {
    | ("", _) => ""
    | (value, Percent) => value ++ "%"
    | (value, Pt) => value ++ "pt"
    | (value, Px) => value ++ "px"
    }
  }

  let updateBackend = (formData: formData, formMetrics: formMetrics) => {
    let jsonData = Js.Dict.empty()
    let updateJsonData = (key, value, metric) => {
      Js.Dict.set(jsonData, key, Js.Json.string(value))
      let metricString = switch metric {
      | Px => "px"
      | Pt => "pt"
      | Percent => "%"
      }
      Js.Dict.set(jsonData, key ++ "_metric", Js.Json.string(metricString))
    }
    updateJsonData("margin_top", formData.marginTop, formMetrics.marginTopM)
    updateJsonData("margin_right", formData.marginRight, formMetrics.marginRightM)
    updateJsonData("margin_bottom", formData.marginBottom, formMetrics.marginBottomM)
    updateJsonData("margin_left", formData.marginLeft, formMetrics.marginLeftM)
    updateJsonData("padding_top", formData.paddingTop, formMetrics.paddingTopM)
    updateJsonData("padding_right", formData.paddingRight, formMetrics.paddingRightM)
    updateJsonData("padding_bottom", formData.paddingBottom, formMetrics.paddingBottomM)
    updateJsonData("padding_left", formData.paddingLeft, formMetrics.paddingLeftM)
    Fetch.updateElementStyles("1", Js.Json.object_(jsonData))
    |> Js.Promise.then_(_ => Js.Promise.resolve())
    |> Js.Promise.catch(error => {
      Js.log("Error updating element styles: " ++ Js.String.make(error))
      Js.Promise.resolve()
    })
    |> ignore
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

    let initialFormMetrics = {
      marginTopM: Px,
      marginRightM: Px,
      marginBottomM: Px,
      marginLeftM: Px,
      paddingTopM: Px,
      paddingRightM: Px,
      paddingBottomM: Px,
      paddingLeftM: Px,
    }

    let (formData, setFormData) = React.useState(_ => initialFormData)
    let (formDataState, setFormDataState) = React.useState(_ => initialFormDataState)
    let (formMetrics, setFormMetrics) = React.useState(_ => initialFormMetrics)

    React.useEffect0(() => {
      let fetchStyles = () => {
        Fetch.getElementStyles("1")
        |> Js.Promise.then_(stylesJson => {
          let styles = Js.Json.decodeObject(stylesJson)->Belt.Option.getExn
          let updatedFormData = {
            marginTop: Js.Dict.get(styles, "margin_top")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            marginRight: Js.Dict.get(styles, "margin_right")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            marginBottom: Js.Dict.get(styles, "margin_bottom")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            marginLeft: Js.Dict.get(styles, "margin_left")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            paddingTop: Js.Dict.get(styles, "padding_top")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            paddingRight: Js.Dict.get(styles, "padding_right")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            paddingBottom: Js.Dict.get(styles, "padding_bottom")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
            paddingLeft: Js.Dict.get(styles, "padding_left")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault(""),
          }
          setFormData(_ => updatedFormData)
          // Update formDataState to Changed for non-empty values
          setFormDataState(prev => {
            marginTopS: updatedFormData.marginTop !== "" ? Changed : Default,
            marginRightS: updatedFormData.marginRight !== "" ? Changed : Default,
            marginBottomS: updatedFormData.marginBottom !== "" ? Changed : Default,
            marginLeftS: updatedFormData.marginLeft !== "" ? Changed : Default,
            paddingTopS: updatedFormData.paddingTop !== "" ? Changed : Default,
            paddingRightS: updatedFormData.paddingRight !== "" ? Changed : Default,
            paddingBottomS: updatedFormData.paddingBottom !== "" ? Changed : Default,
            paddingLeftS: updatedFormData.paddingLeft !== "" ? Changed : Default,
          })
          Js.Promise.resolve()
        })
        |> ignore
      }
      fetchStyles()
      None
    })

    let handleInputChange = (key, value) => {
      let (numericValue, _) = parseValue(value)
      let metric = switch key {
      | "marginTop" => formMetrics.marginTopM
      | "marginRight" => formMetrics.marginRightM
      | "marginBottom" => formMetrics.marginBottomM
      | "marginLeft" => formMetrics.marginLeftM
      | "paddingTop" => formMetrics.paddingTopM
      | "paddingRight" => formMetrics.paddingRightM
      | "paddingBottom" => formMetrics.paddingBottomM
      | "paddingLeft" => formMetrics.paddingLeftM
      | _ => Px
      }
      let formattedValue = formatValue(numericValue, metric)
      setFormData(prev => {
        let updatedFormData = switch key {
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
        updateBackend(updatedFormData, formMetrics)
        updatedFormData
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

    let handleMetricChange = (key, metric) => {
      setFormMetrics(prev => {
        let updatedMetrics = switch key {
        | "marginTop" => {...prev, marginTopM: metric}
        | "marginRight" => {...prev, marginRightM: metric}
        | "marginBottom" => {...prev, marginBottomM: metric}
        | "marginLeft" => {...prev, marginLeftM: metric}
        | "paddingTop" => {...prev, paddingTopM: metric}
        | "paddingRight" => {...prev, paddingRightM: metric}
        | "paddingBottom" => {...prev, paddingBottomM: metric}
        | "paddingLeft" => {...prev, paddingLeftM: metric}
        | _ => prev
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
        let (numericValue, _) = parseValue(value)
        let formattedValue = formatValue(numericValue, metric)
        let updatedFormData = switch key {
        | "marginTop" => {...formData, marginTop: formattedValue}
        | "marginRight" => {...formData, marginRight: formattedValue}
        | "marginBottom" => {...formData, marginBottom: formattedValue}
        | "marginLeft" => {...formData, marginLeft: formattedValue}
        | "paddingTop" => {...formData, paddingTop: formattedValue}
        | "paddingRight" => {...formData, paddingRight: formattedValue}
        | "paddingBottom" => {...formData, paddingBottom: formattedValue}
        | "paddingLeft" => {...formData, paddingLeft: formattedValue}
        | _ => formData
        }
        setFormData(_ => updatedFormData)
        updateBackend(updatedFormData, updatedMetrics)
        updatedMetrics
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

      let currentMetric = switch key {
      | "marginTop" => formMetrics.marginTopM
      | "marginRight" => formMetrics.marginRightM
      | "marginBottom" => formMetrics.marginBottomM
      | "marginLeft" => formMetrics.marginLeftM
      | "paddingTop" => formMetrics.paddingTopM
      | "paddingRight" => formMetrics.paddingRightM
      | "paddingBottom" => formMetrics.paddingBottomM
      | "paddingLeft" => formMetrics.paddingLeftM
      | _ => Px
      }

      <div style={ReactDOM.Style.combine(styles["inputContainer"], style)}>
        {switch stateKey {
        | Focused =>
          <div style={ReactDOM.Style.combine(styles["valueDisplayFocused"], style)}>
            <input
              type_="text"
              style={getInputStyle(stateKey)}
              value={Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)}
              onChange={e => handleInputChange(key, ReactEvent.Form.target(e)["value"])}
              onKeyDown={e => {
                if ReactEvent.Keyboard.key(e) === "Enter" {
                  handleBlur(key)
                }
              }}
              autoFocus={true}
            />
            <select
              style={styles["select"]}
              value={switch currentMetric {
              | Px => "px"
              | Pt => "pt"
              | Percent => "%"
              }}
              onChange={e => {
                let selectedMetric = switch ReactEvent.Form.target(e)["value"] {
                | "px" => Px
                | "pt" => Pt
                | "%" => Percent
                | _ => Px
                }
                handleMetricChange(key, selectedMetric)
              }}
              onMouseDown={e => ReactEvent.Mouse.stopPropagation(e)}
              onClick={e => ReactEvent.Mouse.stopPropagation(e)}>
              <option value="px"> {React.string("px")} </option>
              <option value="pt"> {React.string("pt")} </option>
              <option value="%"> {React.string("%")} </option>
            </select>
          </div>
        | Changed =>
          <div
            style={ReactDOM.Style.combine(styles["valueDisplayChanged"], style)}
            onClick={_ => handleFocus(key)}>
            {React.string(value)}
          </div>
        | Default =>
          <div
            style={ReactDOM.Style.combine(styles["valueDisplayDefault"], style)}
            onClick={_ => handleFocus(key)}>
            {React.string("auto")}
          </div>
        }}
      </div>
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
      </div>
    </div>
  }
}
