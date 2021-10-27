import { dummyLogger, Logger as TsLogger } from 'ts-log'
import util from 'util'

export type Logger = TsLogger

/** @internal */
export const loadLogger = (context: string, logger?: Logger): Logger => {
  if (logger === undefined) return dummyLogger
  const methodNames = ['debug', 'error', 'info', 'trace', 'warn'] as (keyof Logger)[]
  return <Logger>(
    (<unknown>(
      Object.fromEntries(
        methodNames.map((method) => [
          method,
          (message: string, ...optionalParams: any[]) =>
            logger[method](
              util.inspect({ context, ...optionalParams }, false, null, true),
              message
            )
        ])
      )
    ))
  )
}
