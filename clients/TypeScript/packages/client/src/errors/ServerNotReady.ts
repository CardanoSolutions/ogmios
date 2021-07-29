import { CustomError } from 'ts-custom-error'
import { ServerHealth } from '../ServerHealth'

/** @category Connection */
export class ServerNotReady extends CustomError {
  public constructor (health: ServerHealth) {
    super()
    this.message =
      `Server is not ready. Network synchronization at ${health.networkSynchronization}%`
  }
}
