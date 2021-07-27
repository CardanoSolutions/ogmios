import fetch from 'cross-fetch'
import { Connection } from './Connection'
import { Tip } from '@cardano-ogmios/schema'

export interface OgmiosHealth {
  currentEra: 'Alonzo' | 'Byron' | 'Mary' | 'Shelley'
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
  networkSynchronization: number,
}

export const getOgmiosHealth = async (
  connection?: Connection
): Promise<OgmiosHealth> => {
  const response = await fetch(`${connection.address.http}/health`)
  const responseJson = await response.json()
  if (response.ok) {
    return responseJson
  } else {
    throw new Error(response.statusText)
  }
}
