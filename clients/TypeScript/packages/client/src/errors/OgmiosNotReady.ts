import { CustomError } from 'ts-custom-error'
import { OgmiosHealth } from '../OgmiosHealth'

/** @category Connection */
export class OgmiosNotReady extends CustomError {
  public constructor (health: OgmiosHealth) {
    super()
    this.message =
      `Ogmios is not ready. Network synchronization at ${health.networkSynchronization}%`
  }
}
