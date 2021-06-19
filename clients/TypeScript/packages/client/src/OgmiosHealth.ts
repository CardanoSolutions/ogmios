import fetch from 'cross-fetch'
import {
  ConnectionConfig,
  createInteractionContext,
  InteractionContext,
  isContext
} from './Connection'
import { Tip } from '@cardano-ogmios/schema'

export interface OgmiosHealth {
  currentEra: 'Alonzo' | 'Byron' | 'Mary' | 'Shelley'
  lastKnownTip: Tip,
  lastTipUpdate: string | null,
  metrics: {
    runtimeStats: {
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
  networkSynchronization: number,
}

export const getOgmiosHealth = async (
  config?: ConnectionConfig | InteractionContext
): Promise<OgmiosHealth> => {
  const { connection } = isContext(config)
    ? config
    : await createInteractionContext(
      () => {},
      { connection: config }
    )
  const response = await fetch(`${connection.address.http}/health`)
  const responseJson = await response.json()
  if (response.ok) {
    return responseJson
  } else {
    throw new Error(response.statusText)
  }
}
