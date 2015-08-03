module Utils

open System.Configuration
open System
open System.Globalization

let LoadAppSetting<'a>(key : string) : 'a = 
    let value = ConfigurationManager.AppSettings.[key]
    Convert.ChangeType(value, Operators.typeof<'a>, CultureInfo.InvariantCulture) :?> 'a
