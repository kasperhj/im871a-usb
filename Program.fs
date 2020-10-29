open System
open System.IO
open System.IO.Ports
open iM871A_USB.Crc16
open Microsoft.FSharp.Core.LanguagePrimitives

type EndpointIdentifier =
    | DEVMGMT_ID = 0x01uy
    | RADIOLINK_ID = 0x02uy
    | RADIOLINKTEST_ID = 0x03uy
    | HWTEST_ID = 0x04uy
    
type DeviceManagementMessageIdentifier =
    | DEVMGMT_MSG_PING_REQ = 0x01uy
    | DEVMGMT_MSG_PING_RSP = 0x02uy
    | DEVMGMT_MSG_SET_CONFIG_REQ = 0x03uy
    | DEVMGMT_MSG_SET_CONFIG_RSP = 0x04uy
    | DEVMGMT_MSG_GET_CONFIG_REQ = 0x05uy
    | DEVMGMT_MSG_GET_CONFIG_RSP = 0x06uy
    | DEVMGMT_MSG_RESET_REQ = 0x07uy
    | DEVMGMT_MSG_RESET_RSP = 0x08uy
    | DEVMGMT_MSG_FACTORY_RESET_REQ = 0x09uy
    | DEVMGMT_MSG_FACTORY_RESET_RSP = 0x0Auy
    | DEVMGMT_MSG_GET_OPMODE_REQ = 0x0Buy
    | DEVMGMT_MSG_GET_OPMODE_RSP = 0x0Cuy
    | DEVMGMT_MSG_SET_OPMODE_REQ = 0x0Duy
    | DEVMGMT_MSG_SET_OPMODE_RSP = 0x0Euy
    | DEVMGMT_MSG_GET_DEVICEINFO_REQ = 0x0Fuy
    | DEVMGMT_MSG_GET_DEVICEINFO_RSP = 0x10uy
    | DEVMGMT_MSG_GET_SYSSTATUS_REQ = 0x11uy
    | DEVMGMT_MSG_GET_SYSSTATUS_RSP = 0x12uy
    | DEVMGMT_MSG_GET_FWINFO_REQ = 0x13uy
    | DEVMGMT_MSG_GET_FWINFO_RSP = 0x14uy
    | DEVMGMT_MSG_GET_RTC_REQ = 0x19uy
    | DEVMGMT_MSG_GET_RTC_RSP = 0x1Auy
    | DEVMGMT_MSG_SET_RTC_REQ = 0x1Buy
    | DEVMGMT_MSG_SET_RTC_RSP = 0x1Cuy
    | DEVMGMT_MSG_ENTER_LPM_REQ = 0x1Duy
    | DEVMGMT_MSG_ENTER_LPM_RSP = 0x1Euy
    | DEVMGMT_MSG_SET_AES_ENCKEY_REQ = 0x21uy
    | DEVMGMT_MSG_SET_AES_ENCKEY_RSP = 0x22uy
    | DEVMGMT_MSG_ENABLE_AES_ENCKEY_REQ = 0x23uy
    | DEVMGMT_MSG_ENABLE_AES_ENCKEY_RSP = 0x24uy
    | DEVMGMT_MSG_SET_AES_DECKEY_REQ = 0x25uy
    | DEVMGMT_MSG_SET_AES_DECKEY_RSP = 0x26uy
    | DEVMGMT_MSG_AES_DEC_ERROR_IND = 0x27uy
    
type RadioLinkMessageIdentifier =
    | RADIOLINK_MSG_WMBUSMSG_REQ = 0x01uy
    | RADIOLINK_MSG_WMBUSMSG_RSP = 0x02uy
    | RADIOLINK_MSG_WMBUSMSG_IND = 0x03uy
    | RADIOLINK_MSG_DATA_REQ = 0x04uy
    | RADIOLINK_MSG_DATA_RSP = 0x05uy
    
type RadioLinkTestMessageIdentifier =
    | RADIOLINKTEST_MSG_START_REQ = 0x01uy
    | RADIOLINKTEST_MSG_START_RSP = 0x02uy
    | RADIOLINKTEST_MSG_STOP_REQ = 0x03uy
    | RADIOLINKTEST_MSG_STOP_RSP = 0x04uy
    | RADIOLINKTEST_MSG_STATUS_IND = 0x07uy
    
type HardwareTestMessageIdentifier = 
    | HWTEST_MSG_RADIOTEST_REQ = 0x01uy
    | HWTEST_MSG_RADIOTEST_RSP = 0x02uy

module HostControllerInterface =

    [<System.FlagsAttribute>]
    type ControlField =
        | Reserved     = 0b0000uy
        | HasTimestamp = 0b0010uy
        | HasRssi      = 0b0100uy
        | HasCrc16     = 0b1000uy
        
    let SOF: byte = 0xA5uy
       
    type ModuleType =
        | iM871A = 0x33uy
        | iM170A = 0x36uy
        
    type DeviceMode =
        | Other = 0x00uy
        | Meter = 0x01uy
    
    type DeviceInformationResponse =
        { ModuleType: ModuleType
          DeviceMode: DeviceMode
          FirmwareVersion: byte
          HciProtocolVersion: byte
          DeviceId: int }
        
    type PayloadResponse =
        | DeviceInformationResponse of DeviceInformationResponse
        | Empty
        
    type CommandMessage =
        { ControlField: ControlField
          EndpointId: EndpointIdentifier
          MessageId: DeviceManagementMessageIdentifier
          Payload : byte array option }
        
    type ResponseMessage =
        { ControlField: ControlField
          EndpointId: EndpointIdentifier
          MessageId: DeviceManagementMessageIdentifier
          PayloadResponse: PayloadResponse option
          Timestamp: byte array option
          Rssi: byte option
          CrcOk: bool option }
    
    let toBytes (commandMessage : CommandMessage) =
        let controlFieldAndEndpointId = byte commandMessage.ControlField <<< 4 ||| byte commandMessage.EndpointId
        
        let addPayload (payload : byte array option): byte list =
            match payload with
            | Some p -> byte p.Length :: (p |> Array.toList)
            | None -> [0uy]
            
        let bytes = [SOF; controlFieldAndEndpointId; byte commandMessage.MessageId] @ addPayload(commandMessage.Payload)
        
        if commandMessage.ControlField.HasFlag(ControlField.HasCrc16)
        then bytes @ (Crc16.computeBytes bytes |> List.ofArray) |> List.toArray
        else bytes |> List.toArray
    
    let ping () =
        { ControlField = ControlField.Reserved;
          EndpointId = EndpointIdentifier.DEVMGMT_ID;
          MessageId = DeviceManagementMessageIdentifier.DEVMGMT_MSG_PING_REQ;
          Payload = None; }
        
    let reset () =
        { ControlField = ControlField.Reserved;
          EndpointId = EndpointIdentifier.DEVMGMT_ID;
          MessageId = DeviceManagementMessageIdentifier.DEVMGMT_MSG_RESET_REQ;
          Payload = None; }
        
    let info () =
        { ControlField = ControlField.Reserved;
          EndpointId = EndpointIdentifier.DEVMGMT_ID;
          MessageId = DeviceManagementMessageIdentifier.DEVMGMT_MSG_GET_DEVICEINFO_REQ;
          Payload = None; }
               
    let private isCrcOk(bytes: byte array, crcBytes: byte array) =
        Crc16.computeBytes(bytes) |> Array.rev = crcBytes
        
    let private createPayloadResponse(messageId: DeviceManagementMessageIdentifier, payload: byte array) =
        match messageId with
        | DeviceManagementMessageIdentifier.DEVMGMT_MSG_GET_DEVICEINFO_RSP ->
            DeviceInformationResponse
                { ModuleType = EnumOfValue<byte, ModuleType>(payload.[0]);
                  DeviceMode = EnumOfValue<byte, DeviceMode>(payload.[1])
                  FirmwareVersion = payload.[2];
                  HciProtocolVersion = payload.[3];
                  DeviceId = BitConverter.ToInt32(payload.[4..], 0) }
        | _ -> raise <| NotImplementedException("")

    let streamReader(stream: Stream, messageHandler: ResponseMessage -> unit) =
        // Align to the Start of frame
        let firstByte = byte (stream.ReadByte())
        
        //if firstByte = SOF then
        // When we have found the start, we know calculate how much to read until a complete
        // message has been received based on the Control field and Payload length.
       
        let buffer = Array.create 1024 0uy
            
        async {
            // Read until we know the payload length
            let! bytesRead = stream.ReadAsync(buffer, 0, 3) |> Async.AwaitTask
                    
            let controlField: ControlField = EnumOfValue<byte, ControlField>(byte (buffer.[0] >>> 4))
            let endpointId = EnumOfValue<byte, EndpointIdentifier>(buffer.[0] &&& 0x0Fuy)
            let messageId = EnumOfValue<byte, DeviceManagementMessageIdentifier>(buffer.[1])
            let payloadLength = int buffer.[2]
            
            let mutable messageLength = int payloadLength
            
            let hasCrc = controlField.HasFlag(ControlField.HasCrc16)
            let hasTimestamp = controlField.HasFlag(ControlField.HasTimestamp)
            let hasRssi = controlField.HasFlag(ControlField.HasRssi)
            
            if hasTimestamp then messageLength <- messageLength + 4
            if hasRssi then messageLength <- messageLength + 1
            if hasCrc then messageLength <- messageLength + 2
                       
            // Now we know the full message length, so read the entire message
            let! bytesRead = stream.ReadAsync(buffer, 3, messageLength) |> Async.AwaitTask
            let messageBytes = Array.create (messageLength+3) 0uy           
            Buffer.BlockCopy(buffer, 0, messageBytes, 0, bytesRead+3)

            let response =
                     { ResponseMessage.ControlField = controlField
                       ResponseMessage.EndpointId = endpointId
                       ResponseMessage.MessageId = messageId
                       ResponseMessage.PayloadResponse = if payloadLength > 0 then Some (createPayloadResponse(messageId, messageBytes.[3..payloadLength+2])) else None
                       ResponseMessage.CrcOk = if hasCrc then Some (isCrcOk(messageBytes.[0..messageBytes.Length-3], messageBytes.[messageBytes.Length-2..])) else None
                       ResponseMessage.Rssi = None
                       ResponseMessage.Timestamp = None }

            messageHandler response
        }

[<EntryPoint>]
let main argv =
    let port = new SerialPort("/dev/ttyUSB0", 57600)
    port.Open()
    port.DataReceived.AddHandler(fun obj args -> HostControllerInterface.streamReader(port.BaseStream, fun response -> printfn "%A" response) |> Async.RunSynchronously)
    
    let send(bytes: byte array) = 
        port.BaseStream.WriteAsync(bytes, 0, bytes.Length)  
        |> Async.AwaitTask
        |> Async.RunSynchronously
        
    let bytes =
        HostControllerInterface.ping() |> HostControllerInterface.toBytes 
    
    printfn "%A" bytes

    send(bytes)    
    
    printfn "Listening..."
    
    //reader()
    
    let rec readKey() =
        let key = Console.ReadKey()
        if key.KeyChar <> 'q' then 
            send(HostControllerInterface.info() |> HostControllerInterface.toBytes)
            readKey()
    
    readKey()
    
    printfn "Done."
    
    port.Close()
    0
