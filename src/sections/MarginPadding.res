// Import the CSS module
%raw(`require("./MarginPadding.css")`)

module ControlPaddingAndMargin = {
  type inputState = Default | Changed | Focused
  type metric = Px | Pt | Percent
  type side = Top | Right | Bottom | Left
  type property = Margin | Padding

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

  let getPropertyKey = (property, side) =>
    switch (property, side) {
    | (Margin, Top) => "margin_top"
    | (Margin, Right) => "margin_right"
    | (Margin, Bottom) => "margin_bottom"
    | (Margin, Left) => "margin_left"
    | (Padding, Top) => "padding_top"
    | (Padding, Right) => "padding_right"
    | (Padding, Bottom) => "padding_bottom"
    | (Padding, Left) => "padding_left"
    }

  let updateBackend = (formData: formData, formMetrics: formMetrics) => {
    let jsonData = Js.Dict.empty()
    let updateJsonData = (property, side, value, metric) => {
      let key = getPropertyKey(property, side)
      Js.Dict.set(jsonData, key, Js.Json.string(value))
      Js.Dict.set(jsonData, key ++ "_metric", Js.Json.string(getMetricString(metric)))
    }

    // Update margins
    updateJsonData(Margin, Top, formData.marginTop, formMetrics.marginTopM)
    updateJsonData(Margin, Right, formData.marginRight, formMetrics.marginRightM)
    updateJsonData(Margin, Bottom, formData.marginBottom, formMetrics.marginBottomM)
    updateJsonData(Margin, Left, formData.marginLeft, formMetrics.marginLeftM)

    // Update paddings
    updateJsonData(Padding, Top, formData.paddingTop, formMetrics.paddingTopM)
    updateJsonData(Padding, Right, formData.paddingRight, formMetrics.paddingRightM)
    updateJsonData(Padding, Bottom, formData.paddingBottom, formMetrics.paddingBottomM)
    updateJsonData(Padding, Left, formData.paddingLeft, formMetrics.paddingLeftM)

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
          let getValue = key =>
            Js.Dict.get(styles, key)
            ->Belt.Option.flatMap(Js.Json.decodeString)
            ->Belt.Option.getWithDefault("")

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

          setFormData(_ => updatedFormData)
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
        updateBackend(updatedFormData, formMetrics)
        updatedFormData
      })
    }

    let handleFocus = key =>
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

    let handleBlur = key =>
      setFormDataState(prev =>
        switch key {
        | "marginTop" => {...prev, marginTopS: formData.marginTop !== "" ? Changed : Default}
        | "marginRight" => {...prev, marginRightS: formData.marginRight !== "" ? Changed : Default}
        | "marginBottom" => {...prev, marginBottomS: formData.marginBottom !== "" ? Changed : Default}
        | "marginLeft" => {...prev, marginLeftS: formData.marginLeft !== "" ? Changed : Default}
        | "paddingTop" => {...prev, paddingTopS: formData.paddingTop !== "" ? Changed : Default}
        | "paddingRight" => {...prev, paddingRightS: formData.paddingRight !== "" ? Changed : Default}
        | "paddingBottom" => {...prev, paddingBottomS: formData.paddingBottom !== "" ? Changed : Default}
        | "paddingLeft" => {...prev, paddingLeftS: formData.paddingLeft !== "" ? Changed : Default}
        | _ => prev
        }
      )

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

      <div className={`inputContainer ${className}`}>
        {switch stateKey {
        | Focused =>
          <div className={`valueDisplayFocused ${className}`}>
            <input
              type_="text"
              className={getInputStyle(stateKey)}
              value={Js.String.replaceByRe(%re("/[^0-9]/g"), "", value)}
              onChange={e => handleInputChange(key, ReactEvent.Form.target(e)["value"])}
              onKeyDown={e => ReactEvent.Keyboard.key(e) === "Enter" ? handleBlur(key) : ()}
              autoFocus={true}
            />
            <select
              className="select"
              value={getMetricString(currentMetric)}
              onChange={e => {
                let selectedMetric = switch ReactEvent.Form.target(e)["value"] {
                | "px" => Px
                | "pt" => Pt
                | "%" => Percent
                | _ => Px
                }
                handleMetricChange(key, selectedMetric)
              }}
              onMouseDown={ReactEvent.Mouse.stopPropagation}
              onClick={ReactEvent.Mouse.stopPropagation}>
              <option value="px"> {React.string("px")} </option>
              <option value="pt"> {React.string("pt")} </option>
              <option value="%"> {React.string("%")} </option>
            </select>
          </div>
        | Changed =>
          <div
            className={`valueDisplayChanged ${className}`}
            onClick={_ => handleFocus(key)}>
            {React.string(value)}
          </div>
        | Default =>
          <div
            className={`valueDisplayDefault ${className}`}
            onClick={_ => handleFocus(key)}>
            {React.string("auto")}
          </div>
        }}
      </div>
    }

    <div className="boxContainer">
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
