// Import the CSS module
%raw(`require("./MarginPadding.css")`)

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

  let getInputStyle = state => {
    switch state {
    | Default => "input"
    | Changed => "input"
    | Focused => "input inputFocused"
    }
  }

  let parseValue = value => {
    let numericValue = Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)
    let metric = switch true {
    | _ if Js.String.includes("%", value) => Percent
    | _ if Js.String.includes("pt", value) => Pt
    | _ => Px
    }
    (numericValue, metric)
  }

  let formatValue = (value, metric) =>
    switch (value, metric) {
    | ("", _) => ""
    | (value, Percent) => value ++ "%"
    | (value, Pt) => value ++ "pt"
    | (value, Px) => value ++ "px"
    }

  let getMetricString = metric =>
    switch metric {
    | Px => "px"
    | Pt => "pt"
    | Percent => "%"
    }

  let getMetricFromString = str =>
    switch str {
    | "px" => Px
    | "pt" => Pt
    | "%" => Percent
    | _ => Px
    }

  let updateBackend = (formData: formData) => {
    let jsonData = Js.Dict.empty()

    Js.Dict.set(jsonData, "margin_top", Js.Json.string(formData.marginTop))
    Js.Dict.set(jsonData, "margin_right", Js.Json.string(formData.marginRight))
    Js.Dict.set(jsonData, "margin_bottom", Js.Json.string(formData.marginBottom))
    Js.Dict.set(jsonData, "margin_left", Js.Json.string(formData.marginLeft))
    Js.Dict.set(jsonData, "padding_top", Js.Json.string(formData.paddingTop))
    Js.Dict.set(jsonData, "padding_right", Js.Json.string(formData.paddingRight))
    Js.Dict.set(jsonData, "padding_bottom", Js.Json.string(formData.paddingBottom))
    Js.Dict.set(jsonData, "padding_left", Js.Json.string(formData.paddingLeft))

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
    let (activeKey, setActiveKey) = React.useState(_ => None)

    React.useEffect0(() => {
      let fetchStyles = () => {
        Fetch.getElementStyles("1")
        |> Js.Promise.then_(stylesJson => {
          let styles = Js.Json.decodeObject(stylesJson)->Belt.Option.getExn
          let getValue = key =>
            Js.Dict.get(styles, key)
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault("")

          let getMetricValue = key =>
            Js.Dict.get(styles, key ++ "_metric")
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.map(getMetricFromString)
            ->Belt.Option.getWithDefault(Px)

          let updatedFormData = {
            marginTop: getValue("margin_top"),
            marginRight: getValue("margin_right"),
            marginBottom: getValue("margin_bottom"),
            marginLeft: getValue("margin_left"),
            paddingTop: getValue("padding_top"),
            paddingRight: getValue("padding_right"),
            paddingBottom: getValue("padding_bottom"),
            paddingLeft: getValue("padding_left"),
          }

          let updatedFormMetrics = {
            marginTopM: getMetricValue("margin_top"),
            marginRightM: getMetricValue("margin_right"),
            marginBottomM: getMetricValue("margin_bottom"),
            marginLeftM: getMetricValue("margin_left"),
            paddingTopM: getMetricValue("padding_top"),
            paddingRightM: getMetricValue("padding_right"),
            paddingBottomM: getMetricValue("padding_bottom"),
            paddingLeftM: getMetricValue("padding_left"),
          }

          setFormData(_ => updatedFormData)
          setFormMetrics(_ => updatedFormMetrics)
          setFormDataState(_ => {
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
        updateBackend(updatedFormData)
        updatedFormData
      })
    }

    let handleBlur = key =>
      setFormDataState(prev =>
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
      )

    let handleFocus = key => {
      switch activeKey {
      | Some(currentKey) =>
        setFormDataState(prev =>
          switch currentKey {
          | "marginTop" => {...prev, marginTopS: formData.marginTop !== "" ? Changed : Default}
          | "marginRight" => {
              ...prev,
              marginRightS: formData.marginRight !== "" ? Changed : Default,
            }
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
          | "paddingLeft" => {
              ...prev,
              paddingLeftS: formData.paddingLeft !== "" ? Changed : Default,
            }
          | _ => prev
          }
        )
      | None => ()
      }

      setActiveKey(_ => Some(key))
      setFormDataState(prev =>
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
      )
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
        updateBackend(updatedFormData)
        updatedMetrics
      })
    }

    let renderInput = (key, className) => {
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

      let (_, valueMetric) = parseValue(value)
      let displayMetric = value !== "" ? valueMetric : currentMetric

      <div className={`inputContainer ${className}`}>
        {switch stateKey {
        | Focused =>
          <div
            className={`valueDisplayFocused ${className}`}
            onClick={e => {
              ReactEvent.Mouse.stopPropagation(e)
            }}>
            <input
              type_="text"
              className={getInputStyle(stateKey)}
              value={Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)}
              onChange={e => handleInputChange(key, ReactEvent.Form.target(e)["value"])}
              onKeyDown={e => ReactEvent.Keyboard.key(e) === "Enter" ? handleBlur(key) : ()}
              autoFocus={true}
              onClick={ReactEvent.Mouse.stopPropagation}
            />
            <select
              className="select"
              value={getMetricString(displayMetric)}
              onChange={e => {
                let selectedMetric = switch ReactEvent.Form.target(e)["value"] {
                | "px" => Px
                | "pt" => Pt
                | "%" => Percent
                | _ => Px
                }
                handleMetricChange(key, selectedMetric)
              }}
              onClick={ReactEvent.Mouse.stopPropagation}>
              <option value="px"> {React.string("px")} </option>
              <option value="pt"> {React.string("pt")} </option>
              <option value="%"> {React.string("%")} </option>
            </select>
          </div>
        | Changed =>
          <div
            className={`valueDisplayChanged ${className}`}
            onClick={e => {
              ReactEvent.Mouse.stopPropagation(e)
              handleFocus(key)
            }}>
            <span className="text-value"> {React.string(value)} </span>
          </div>
        | Default =>
          <div
            className={`valueDisplayDefault ${className}`}
            onClick={e => {
              ReactEvent.Mouse.stopPropagation(e)
              handleFocus(key)
            }}>
            <span className="text-auto"> {React.string("auto")} </span>
          </div>
        }}
      </div>
    }

    <div
      className="boxContainer"
      onClick={_ => {
        switch activeKey {
        | Some(key) => handleBlur(key)
        | None => ()
        }
        setActiveKey(_ => None)
      }}>
      <div className="box">
        {renderInput("marginTop", "marginTop")}
        {renderInput("marginBottom", "marginBottom")}
        {renderInput("marginLeft", "marginLeft")}
        {renderInput("marginRight", "marginRight")}
        {renderInput("paddingTop", "paddingTop")}
        {renderInput("paddingBottom", "paddingBottom")}
        {renderInput("paddingLeft", "paddingLeft")}
        {renderInput("paddingRight", "paddingRight")}
      </div>
    </div>
  }
}
