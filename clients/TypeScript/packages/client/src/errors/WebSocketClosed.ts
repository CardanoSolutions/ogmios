import { CustomError } from 'ts-custom-error'

export class WebSocketClosed extends CustomError {
  public constructor () {
    super()
    this.message = 'WebSocket is closed'
  }
}
