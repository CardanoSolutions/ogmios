import fetch from 'cross-fetch'
import { CustomError } from 'ts-custom-error'
import { Connection } from './Connection'
import { Era, Tip } from '@cardano-ogmios/schema'

/**
 * Captures the health of the server, including metrics and synchronization progress.
 *
 * This is useful to monitor whether the server is running (or simply, if the underlying
 * node is ready to receive requests). `lastTipUpdate` will be null if the server hasn't
 * received any tip update from the Cardano node which means that either:
 *
 * - There's an error with the connection to the node
 * - The node is still starting and checking the integrity of its database.
 *
 * The node checks the database integrity on each restart; in some cases it can take several
 * minutes. While doing so, it does not allow any external client to connect.
 *
 * See also {@link InteractionContext} for an easy way to manage connection establishment with
 * a bootstrapping node.
 *
 * @category Connection
 */
export interface ServerHealth {
  currentEra: Era,
  lastKnownTip: Tip,
  lastTipUpdate: string | null,
  metrics: {
    runtimeStats?: {
      gcCpuTime: number,
      cpuTime: number,
      maxHeapSize: number,
      currentHeapSize: number
    },
    sessionDurations: {
      max: number,
      mean: number,
      min: number
    },
    totalConnections: number,
    totalMessages: number,
    totalUnrouted: number,
    activeConnections: number
  },
  startTime: string,
  network: 'mainnet' | 'preview' | 'preprod',
  networkSynchronization: number
  version: string,
}

/**
 * Get the server health. This can be safely polled at regular intervals for monitoring.
 *
 * @category Connection
 */
export const getServerHealth = async (
  options?: {
    connection?: Connection
  }
): Promise<ServerHealth> => {
  const response = await fetch(`${options?.connection?.address.http}/health`)
  const responseJson = await response.json()
  if (response.ok) {
    return responseJson
  } else {
    throw new Error(response.statusText)
  }
}

/** @category Connection */
export class ServerNotReady extends CustomError {
  public constructor (health: ServerHealth) {
    super()
    this.message =
      `Server is not ready. Network synchronization at ${health.networkSynchronization}%`
  }
}
